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


-- | Type 'Overlay.SourceOrderConfig'.
--   Configuration data for drawing the source order of an elements children.
data OverlaySourceOrderConfig = OverlaySourceOrderConfig {
  -- | the color to outline the givent element in.
  overlaySourceOrderConfigParentOutlineColor :: DOMPageNetworkEmulationSecurity.DomRgba,
  -- | the color to outline the child elements in.
  overlaySourceOrderConfigChildOutlineColor :: DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlaySourceOrderConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  OverlaySourceOrderConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type 'Overlay.GridHighlightConfig'.
--   Configuration data for the highlighting of Grid elements.
data OverlayGridHighlightConfig = OverlayGridHighlightConfig {
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
  overlayGridHighlightConfigGridBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The row line color (default: transparent).
  overlayGridHighlightConfigRowLineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The column line color (default: transparent).
  overlayGridHighlightConfigColumnLineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | Whether the grid border is dashed (default: false).
  overlayGridHighlightConfigGridBorderDash :: Maybe Bool,
  -- | Whether row lines are dashed (default: false).
  overlayGridHighlightConfigRowLineDash :: Maybe Bool,
  -- | Whether column lines are dashed (default: false).
  overlayGridHighlightConfigColumnLineDash :: Maybe Bool,
  -- | The row gap highlight fill color (default: transparent).
  overlayGridHighlightConfigRowGapColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The row gap hatching fill color (default: transparent).
  overlayGridHighlightConfigRowHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The column gap highlight fill color (default: transparent).
  overlayGridHighlightConfigColumnGapColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The column gap hatching fill color (default: transparent).
  overlayGridHighlightConfigColumnHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The named grid areas border color (Default: transparent).
  overlayGridHighlightConfigAreaBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The grid container background color (Default: transparent).
  overlayGridHighlightConfigGridBackgroundColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayGridHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



-- | Type 'Overlay.FlexContainerHighlightConfig'.
--   Configuration data for the highlighting of Flex container elements.
data OverlayFlexContainerHighlightConfig = OverlayFlexContainerHighlightConfig {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Overlay.FlexItemHighlightConfig'.
--   Configuration data for the highlighting of Flex item elements.
data OverlayFlexItemHighlightConfig = OverlayFlexItemHighlightConfig {
  -- | Style of the box representing the item's base size
  overlayFlexItemHighlightConfigBaseSizeBox :: Maybe OverlayBoxStyle,
  -- | Style of the border around the box representing the item's base size
  overlayFlexItemHighlightConfigBaseSizeBorder :: Maybe OverlayLineStyle,
  -- | Style of the arrow representing if the item grew or shrank
  overlayFlexItemHighlightConfigFlexibilityArrow :: Maybe OverlayLineStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexItemHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexItemHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Overlay.LineStyle'.
--   Style information for drawing a line.
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
  -- | The color of the line (default: transparent)
  overlayLineStyleColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The line pattern (default: solid)
  overlayLineStylePattern :: OverlayLineStylePattern
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayLineStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  OverlayLineStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Overlay.BoxStyle'.
--   Style information for drawing a box.
data OverlayBoxStyle = OverlayBoxStyle {
  -- | The background color for the box (default: transparent)
  overlayBoxStyleFillColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The hatching color for the box (default: transparent)
  overlayBoxStyleHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayBoxStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  OverlayBoxStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'Overlay.ContrastAlgorithm'.
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



-- | Type 'Overlay.HighlightConfig'.
--   Configuration data for the highlighting of page elements.
data OverlayHighlightConfig = OverlayHighlightConfig {
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
  overlayHighlightConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The padding highlight fill color (default: transparent).
  overlayHighlightConfigPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The border highlight fill color (default: transparent).
  overlayHighlightConfigBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The margin highlight fill color (default: transparent).
  overlayHighlightConfigMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The event target element highlight fill color (default: transparent).
  overlayHighlightConfigEventTargetColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The shape outside fill color (default: transparent).
  overlayHighlightConfigShapeColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The shape margin fill color (default: transparent).
  overlayHighlightConfigShapeMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The grid layout color (default: transparent).
  overlayHighlightConfigCssGridColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  OverlayHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type 'Overlay.ColorFormat'.
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



-- | Type 'Overlay.GridNodeHighlightConfig'.
--   Configurations for Persistent Grid Highlight
data OverlayGridNodeHighlightConfig = OverlayGridNodeHighlightConfig {
  -- | A descriptor for the highlight appearance.
  overlayGridNodeHighlightConfigGridHighlightConfig :: OverlayGridHighlightConfig,
  -- | Identifier of the node to highlight.
  overlayGridNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayGridNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Overlay.FlexNodeHighlightConfig'.
data OverlayFlexNodeHighlightConfig = OverlayFlexNodeHighlightConfig {
  -- | A descriptor for the highlight appearance of flex containers.
  overlayFlexNodeHighlightConfigFlexContainerHighlightConfig :: OverlayFlexContainerHighlightConfig,
  -- | Identifier of the node to highlight.
  overlayFlexNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Type 'Overlay.ScrollSnapContainerHighlightConfig'.
data OverlayScrollSnapContainerHighlightConfig = OverlayScrollSnapContainerHighlightConfig {
  -- | The style of the snapport border (default: transparent)
  overlayScrollSnapContainerHighlightConfigSnapportBorder :: Maybe OverlayLineStyle,
  -- | The style of the snap area border (default: transparent)
  overlayScrollSnapContainerHighlightConfigSnapAreaBorder :: Maybe OverlayLineStyle,
  -- | The margin highlight fill color (default: transparent).
  overlayScrollSnapContainerHighlightConfigScrollMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The padding highlight fill color (default: transparent).
  overlayScrollSnapContainerHighlightConfigScrollPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



-- | Type 'Overlay.ScrollSnapHighlightConfig'.
data OverlayScrollSnapHighlightConfig = OverlayScrollSnapHighlightConfig {
  -- | A descriptor for the highlight appearance of scroll snap containers.
  overlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig :: OverlayScrollSnapContainerHighlightConfig,
  -- | Identifier of the node to highlight.
  overlayScrollSnapHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type 'Overlay.HingeConfig'.
--   Configuration for dual screen hinge
data OverlayHingeConfig = OverlayHingeConfig {
  -- | A rectangle represent hinge
  overlayHingeConfigRect :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | The content box highlight fill color (default: a dark color).
  overlayHingeConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The content box highlight outline color (default: transparent).
  overlayHingeConfigOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHingeConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  OverlayHingeConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Type 'Overlay.ContainerQueryHighlightConfig'.
data OverlayContainerQueryHighlightConfig = OverlayContainerQueryHighlightConfig {
  -- | A descriptor for the highlight appearance of container query containers.
  overlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig :: OverlayContainerQueryContainerHighlightConfig,
  -- | Identifier of the container node to highlight.
  overlayContainerQueryHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



-- | Type 'Overlay.ContainerQueryContainerHighlightConfig'.
data OverlayContainerQueryContainerHighlightConfig = OverlayContainerQueryContainerHighlightConfig {
  -- | The style of the container border.
  overlayContainerQueryContainerHighlightConfigContainerBorder :: Maybe OverlayLineStyle,
  -- | The style of the descendants' borders.
  overlayContainerQueryContainerHighlightConfigDescendantBorder :: Maybe OverlayLineStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



-- | Type 'Overlay.IsolatedElementHighlightConfig'.
data OverlayIsolatedElementHighlightConfig = OverlayIsolatedElementHighlightConfig {
  -- | A descriptor for the highlight appearance of an element in isolation mode.
  overlayIsolatedElementHighlightConfigIsolationModeHighlightConfig :: OverlayIsolationModeHighlightConfig,
  -- | Identifier of the isolated element to highlight.
  overlayIsolatedElementHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolatedElementHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolatedElementHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



-- | Type 'Overlay.IsolationModeHighlightConfig'.
data OverlayIsolationModeHighlightConfig = OverlayIsolationModeHighlightConfig {
  -- | The fill color of the resizers (default: transparent).
  overlayIsolationModeHighlightConfigResizerColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The fill color for resizer handles (default: transparent).
  overlayIsolationModeHighlightConfigResizerHandleColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The fill color for the mask covering non-isolated elements (default: transparent).
  overlayIsolationModeHighlightConfigMaskColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolationModeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolationModeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Overlay.InspectMode'.
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
  -- | Id of the node to inspect.
  overlayInspectNodeRequestedBackendNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayInspectNodeRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  OverlayInspectNodeRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type of the 'Overlay.nodeHighlightRequested' event.
data OverlayNodeHighlightRequested = OverlayNodeHighlightRequested {
  overlayNodeHighlightRequestedNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayNodeHighlightRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  OverlayNodeHighlightRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



-- | Type of the 'Overlay.screenshotRequested' event.
data OverlayScreenshotRequested = OverlayScreenshotRequested {
  -- | Viewport to capture, in device independent pixels (dip).
  overlayScreenshotRequestedViewport :: DOMPageNetworkEmulationSecurity.PageViewport
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





-- | Function for the 'Overlay.disable' command.
--   Disables domain notifications.
overlayDisable :: Handle ev -> IO ()
overlayDisable handle = sendReceiveCommand handle "Overlay.disable" (Nothing :: Maybe ())


-- | Function for the 'Overlay.enable' command.
--   Enables domain notifications.
overlayEnable :: Handle ev -> IO ()
overlayEnable handle = sendReceiveCommand handle "Overlay.enable" (Nothing :: Maybe ())


-- | Parameters of the 'overlayGetHighlightObjectForTest' command.
data POverlayGetHighlightObjectForTest = POverlayGetHighlightObjectForTest {
  -- | Id of the node to get highlight object for.
  pOverlayGetHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Whether to include distance info.
  pOverlayGetHighlightObjectForTestIncludeDistance :: Maybe Bool,
  -- | Whether to include style info.
  pOverlayGetHighlightObjectForTestIncludeStyle :: Maybe Bool,
  -- | The color format to get config with (default: hex).
  pOverlayGetHighlightObjectForTestColorFormat :: Maybe OverlayColorFormat,
  -- | Whether to show accessibility info (default: true).
  pOverlayGetHighlightObjectForTestShowAccessibilityInfo :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Overlay.getHighlightObjectForTest' command.
--   For testing.
--   Parameters: 'POverlayGetHighlightObjectForTest'
--   Returns: 'OverlayGetHighlightObjectForTest'
overlayGetHighlightObjectForTest :: Handle ev -> POverlayGetHighlightObjectForTest -> IO OverlayGetHighlightObjectForTest
overlayGetHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getHighlightObjectForTest" (Just params)

-- | Return type of the 'overlayGetHighlightObjectForTest' command.
data OverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest {
  -- | Highlight data for the node.
  overlayGetHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance Command OverlayGetHighlightObjectForTest where
   commandName _ = "Overlay.getHighlightObjectForTest"



-- | Parameters of the 'overlayGetGridHighlightObjectsForTest' command.
data POverlayGetGridHighlightObjectsForTest = POverlayGetGridHighlightObjectsForTest {
  -- | Ids of the node to get highlight object for.
  pOverlayGetGridHighlightObjectsForTestNodeIds :: [DOMPageNetworkEmulationSecurity.DomNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetGridHighlightObjectsForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  POverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'Overlay.getGridHighlightObjectsForTest' command.
--   For Persistent Grid testing.
--   Parameters: 'POverlayGetGridHighlightObjectsForTest'
--   Returns: 'OverlayGetGridHighlightObjectsForTest'
overlayGetGridHighlightObjectsForTest :: Handle ev -> POverlayGetGridHighlightObjectsForTest -> IO OverlayGetGridHighlightObjectsForTest
overlayGetGridHighlightObjectsForTest handle params = sendReceiveCommandResult handle "Overlay.getGridHighlightObjectsForTest" (Just params)

-- | Return type of the 'overlayGetGridHighlightObjectsForTest' command.
data OverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest {
  -- | Grid Highlight data for the node ids provided.
  overlayGetGridHighlightObjectsForTestHighlights :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command OverlayGetGridHighlightObjectsForTest where
   commandName _ = "Overlay.getGridHighlightObjectsForTest"



-- | Parameters of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data POverlayGetSourceOrderHighlightObjectForTest = POverlayGetSourceOrderHighlightObjectForTest {
  -- | Id of the node to highlight.
  pOverlayGetSourceOrderHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetSourceOrderHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  POverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }


-- | Function for the 'Overlay.getSourceOrderHighlightObjectForTest' command.
--   For Source Order Viewer testing.
--   Parameters: 'POverlayGetSourceOrderHighlightObjectForTest'
--   Returns: 'OverlayGetSourceOrderHighlightObjectForTest'
overlayGetSourceOrderHighlightObjectForTest :: Handle ev -> POverlayGetSourceOrderHighlightObjectForTest -> IO OverlayGetSourceOrderHighlightObjectForTest
overlayGetSourceOrderHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getSourceOrderHighlightObjectForTest" (Just params)

-- | Return type of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data OverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest {
  -- | Source order highlight data for the node id provided.
  overlayGetSourceOrderHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }

instance Command OverlayGetSourceOrderHighlightObjectForTest where
   commandName _ = "Overlay.getSourceOrderHighlightObjectForTest"



-- | Function for the 'Overlay.hideHighlight' command.
--   Hides any highlight.
overlayHideHighlight :: Handle ev -> IO ()
overlayHideHighlight handle = sendReceiveCommand handle "Overlay.hideHighlight" (Nothing :: Maybe ())


-- | Parameters of the 'overlayHighlightNode' command.
data POverlayHighlightNode = POverlayHighlightNode {
  -- | A descriptor for the highlight appearance.
  pOverlayHighlightNodeHighlightConfig :: OverlayHighlightConfig,
  -- | Identifier of the node to highlight.
  pOverlayHighlightNodeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Identifier of the backend node to highlight.
  pOverlayHighlightNodeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  -- | JavaScript object id of the node to be highlighted.
  pOverlayHighlightNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
  -- | Selectors to highlight relevant nodes.
  pOverlayHighlightNodeSelector :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Overlay.highlightNode' command.
--   Highlights DOM node with given id or with the given JavaScript object wrapper. Either nodeId or
--   objectId must be specified.
--   Parameters: 'POverlayHighlightNode'
overlayHighlightNode :: Handle ev -> POverlayHighlightNode -> IO ()
overlayHighlightNode handle params = sendReceiveCommand handle "Overlay.highlightNode" (Just params)


-- | Parameters of the 'overlayHighlightQuad' command.
data POverlayHighlightQuad = POverlayHighlightQuad {
  -- | Quad to highlight
  pOverlayHighlightQuadQuad :: DOMPageNetworkEmulationSecurity.DomQuad,
  -- | The highlight fill color (default: transparent).
  pOverlayHighlightQuadColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The highlight outline color (default: transparent).
  pOverlayHighlightQuadOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightQuad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightQuad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Overlay.highlightQuad' command.
--   Highlights given quad. Coordinates are absolute with respect to the main frame viewport.
--   Parameters: 'POverlayHighlightQuad'
overlayHighlightQuad :: Handle ev -> POverlayHighlightQuad -> IO ()
overlayHighlightQuad handle params = sendReceiveCommand handle "Overlay.highlightQuad" (Just params)


-- | Parameters of the 'overlayHighlightRect' command.
data POverlayHighlightRect = POverlayHighlightRect {
  -- | X coordinate
  pOverlayHighlightRectX :: Int,
  -- | Y coordinate
  pOverlayHighlightRectY :: Int,
  -- | Rectangle width
  pOverlayHighlightRectWidth :: Int,
  -- | Rectangle height
  pOverlayHighlightRectHeight :: Int,
  -- | The highlight fill color (default: transparent).
  pOverlayHighlightRectColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
  -- | The highlight outline color (default: transparent).
  pOverlayHighlightRectOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Overlay.highlightRect' command.
--   Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.
--   Parameters: 'POverlayHighlightRect'
overlayHighlightRect :: Handle ev -> POverlayHighlightRect -> IO ()
overlayHighlightRect handle params = sendReceiveCommand handle "Overlay.highlightRect" (Just params)


-- | Parameters of the 'overlayHighlightSourceOrder' command.
data POverlayHighlightSourceOrder = POverlayHighlightSourceOrder {
  -- | A descriptor for the appearance of the overlay drawing.
  pOverlayHighlightSourceOrderSourceOrderConfig :: OverlaySourceOrderConfig,
  -- | Identifier of the node to highlight.
  pOverlayHighlightSourceOrderNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
  -- | Identifier of the backend node to highlight.
  pOverlayHighlightSourceOrderBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
  -- | JavaScript object id of the node to be highlighted.
  pOverlayHighlightSourceOrderObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightSourceOrder  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightSourceOrder where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Overlay.highlightSourceOrder' command.
--   Highlights the source order of the children of the DOM node with given id or with the given
--   JavaScript object wrapper. Either nodeId or objectId must be specified.
--   Parameters: 'POverlayHighlightSourceOrder'
overlayHighlightSourceOrder :: Handle ev -> POverlayHighlightSourceOrder -> IO ()
overlayHighlightSourceOrder handle params = sendReceiveCommand handle "Overlay.highlightSourceOrder" (Just params)


-- | Parameters of the 'overlaySetInspectMode' command.
data POverlaySetInspectMode = POverlaySetInspectMode {
  -- | Set an inspection mode.
  pOverlaySetInspectModeMode :: OverlayInspectMode,
  -- | A descriptor for the highlight appearance of hovered-over nodes. May be omitted if `enabled
  --   == false`.
  pOverlaySetInspectModeHighlightConfig :: Maybe OverlayHighlightConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetInspectMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  POverlaySetInspectMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Overlay.setInspectMode' command.
--   Enters the 'inspect' mode. In this mode, elements that user is hovering over are highlighted.
--   Backend then generates 'inspectNodeRequested' event upon element selection.
--   Parameters: 'POverlaySetInspectMode'
overlaySetInspectMode :: Handle ev -> POverlaySetInspectMode -> IO ()
overlaySetInspectMode handle params = sendReceiveCommand handle "Overlay.setInspectMode" (Just params)


-- | Parameters of the 'overlaySetShowAdHighlights' command.
data POverlaySetShowAdHighlights = POverlaySetShowAdHighlights {
  -- | True for showing ad highlights
  pOverlaySetShowAdHighlightsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowAdHighlights  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowAdHighlights where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Overlay.setShowAdHighlights' command.
--   Highlights owner element of all frames detected to be ads.
--   Parameters: 'POverlaySetShowAdHighlights'
overlaySetShowAdHighlights :: Handle ev -> POverlaySetShowAdHighlights -> IO ()
overlaySetShowAdHighlights handle params = sendReceiveCommand handle "Overlay.setShowAdHighlights" (Just params)


-- | Parameters of the 'overlaySetPausedInDebuggerMessage' command.
data POverlaySetPausedInDebuggerMessage = POverlaySetPausedInDebuggerMessage {
  -- | The message to display, also triggers resume and step over controls.
  pOverlaySetPausedInDebuggerMessageMessage :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetPausedInDebuggerMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  POverlaySetPausedInDebuggerMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


-- | Function for the 'Overlay.setPausedInDebuggerMessage' command.
--   
--   Parameters: 'POverlaySetPausedInDebuggerMessage'
overlaySetPausedInDebuggerMessage :: Handle ev -> POverlaySetPausedInDebuggerMessage -> IO ()
overlaySetPausedInDebuggerMessage handle params = sendReceiveCommand handle "Overlay.setPausedInDebuggerMessage" (Just params)


-- | Parameters of the 'overlaySetShowDebugBorders' command.
data POverlaySetShowDebugBorders = POverlaySetShowDebugBorders {
  -- | True for showing debug borders
  pOverlaySetShowDebugBordersShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowDebugBorders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowDebugBorders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Overlay.setShowDebugBorders' command.
--   Requests that backend shows debug borders on layers
--   Parameters: 'POverlaySetShowDebugBorders'
overlaySetShowDebugBorders :: Handle ev -> POverlaySetShowDebugBorders -> IO ()
overlaySetShowDebugBorders handle params = sendReceiveCommand handle "Overlay.setShowDebugBorders" (Just params)


-- | Parameters of the 'overlaySetShowFpsCounter' command.
data POverlaySetShowFpsCounter = POverlaySetShowFpsCounter {
  -- | True for showing the FPS counter
  pOverlaySetShowFpsCounterShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFpsCounter  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFpsCounter where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Overlay.setShowFPSCounter' command.
--   Requests that backend shows the FPS counter
--   Parameters: 'POverlaySetShowFpsCounter'
overlaySetShowFpsCounter :: Handle ev -> POverlaySetShowFpsCounter -> IO ()
overlaySetShowFpsCounter handle params = sendReceiveCommand handle "Overlay.setShowFPSCounter" (Just params)


-- | Parameters of the 'overlaySetShowGridOverlays' command.
data POverlaySetShowGridOverlays = POverlaySetShowGridOverlays {
  -- | An array of node identifiers and descriptors for the highlight appearance.
  pOverlaySetShowGridOverlaysGridNodeHighlightConfigs :: [OverlayGridNodeHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowGridOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowGridOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Overlay.setShowGridOverlays' command.
--   Highlight multiple elements with the CSS Grid overlay.
--   Parameters: 'POverlaySetShowGridOverlays'
overlaySetShowGridOverlays :: Handle ev -> POverlaySetShowGridOverlays -> IO ()
overlaySetShowGridOverlays handle params = sendReceiveCommand handle "Overlay.setShowGridOverlays" (Just params)


-- | Parameters of the 'overlaySetShowFlexOverlays' command.
data POverlaySetShowFlexOverlays = POverlaySetShowFlexOverlays {
  -- | An array of node identifiers and descriptors for the highlight appearance.
  pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs :: [OverlayFlexNodeHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFlexOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFlexOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Overlay.setShowFlexOverlays' command.
--   
--   Parameters: 'POverlaySetShowFlexOverlays'
overlaySetShowFlexOverlays :: Handle ev -> POverlaySetShowFlexOverlays -> IO ()
overlaySetShowFlexOverlays handle params = sendReceiveCommand handle "Overlay.setShowFlexOverlays" (Just params)


-- | Parameters of the 'overlaySetShowScrollSnapOverlays' command.
data POverlaySetShowScrollSnapOverlays = POverlaySetShowScrollSnapOverlays {
  -- | An array of node identifiers and descriptors for the highlight appearance.
  pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs :: [OverlayScrollSnapHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollSnapOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollSnapOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Overlay.setShowScrollSnapOverlays' command.
--   
--   Parameters: 'POverlaySetShowScrollSnapOverlays'
overlaySetShowScrollSnapOverlays :: Handle ev -> POverlaySetShowScrollSnapOverlays -> IO ()
overlaySetShowScrollSnapOverlays handle params = sendReceiveCommand handle "Overlay.setShowScrollSnapOverlays" (Just params)


-- | Parameters of the 'overlaySetShowContainerQueryOverlays' command.
data POverlaySetShowContainerQueryOverlays = POverlaySetShowContainerQueryOverlays {
  -- | An array of node identifiers and descriptors for the highlight appearance.
  pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs :: [OverlayContainerQueryHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowContainerQueryOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowContainerQueryOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


-- | Function for the 'Overlay.setShowContainerQueryOverlays' command.
--   
--   Parameters: 'POverlaySetShowContainerQueryOverlays'
overlaySetShowContainerQueryOverlays :: Handle ev -> POverlaySetShowContainerQueryOverlays -> IO ()
overlaySetShowContainerQueryOverlays handle params = sendReceiveCommand handle "Overlay.setShowContainerQueryOverlays" (Just params)


-- | Parameters of the 'overlaySetShowPaintRects' command.
data POverlaySetShowPaintRects = POverlaySetShowPaintRects {
  -- | True for showing paint rectangles
  pOverlaySetShowPaintRectsResult :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowPaintRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowPaintRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Overlay.setShowPaintRects' command.
--   Requests that backend shows paint rectangles
--   Parameters: 'POverlaySetShowPaintRects'
overlaySetShowPaintRects :: Handle ev -> POverlaySetShowPaintRects -> IO ()
overlaySetShowPaintRects handle params = sendReceiveCommand handle "Overlay.setShowPaintRects" (Just params)


-- | Parameters of the 'overlaySetShowLayoutShiftRegions' command.
data POverlaySetShowLayoutShiftRegions = POverlaySetShowLayoutShiftRegions {
  -- | True for showing layout shift regions
  pOverlaySetShowLayoutShiftRegionsResult :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowLayoutShiftRegions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowLayoutShiftRegions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Overlay.setShowLayoutShiftRegions' command.
--   Requests that backend shows layout shift regions
--   Parameters: 'POverlaySetShowLayoutShiftRegions'
overlaySetShowLayoutShiftRegions :: Handle ev -> POverlaySetShowLayoutShiftRegions -> IO ()
overlaySetShowLayoutShiftRegions handle params = sendReceiveCommand handle "Overlay.setShowLayoutShiftRegions" (Just params)


-- | Parameters of the 'overlaySetShowScrollBottleneckRects' command.
data POverlaySetShowScrollBottleneckRects = POverlaySetShowScrollBottleneckRects {
  -- | True for showing scroll bottleneck rects
  pOverlaySetShowScrollBottleneckRectsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollBottleneckRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollBottleneckRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


-- | Function for the 'Overlay.setShowScrollBottleneckRects' command.
--   Requests that backend shows scroll bottleneck rects
--   Parameters: 'POverlaySetShowScrollBottleneckRects'
overlaySetShowScrollBottleneckRects :: Handle ev -> POverlaySetShowScrollBottleneckRects -> IO ()
overlaySetShowScrollBottleneckRects handle params = sendReceiveCommand handle "Overlay.setShowScrollBottleneckRects" (Just params)


-- | Parameters of the 'overlaySetShowWebVitals' command.
data POverlaySetShowWebVitals = POverlaySetShowWebVitals {
  pOverlaySetShowWebVitalsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowWebVitals  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowWebVitals where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Overlay.setShowWebVitals' command.
--   Request that backend shows an overlay with web vital metrics.
--   Parameters: 'POverlaySetShowWebVitals'
overlaySetShowWebVitals :: Handle ev -> POverlaySetShowWebVitals -> IO ()
overlaySetShowWebVitals handle params = sendReceiveCommand handle "Overlay.setShowWebVitals" (Just params)


-- | Parameters of the 'overlaySetShowViewportSizeOnResize' command.
data POverlaySetShowViewportSizeOnResize = POverlaySetShowViewportSizeOnResize {
  -- | Whether to paint size or not.
  pOverlaySetShowViewportSizeOnResizeShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowViewportSizeOnResize  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowViewportSizeOnResize where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the 'Overlay.setShowViewportSizeOnResize' command.
--   Paints viewport size upon main frame resize.
--   Parameters: 'POverlaySetShowViewportSizeOnResize'
overlaySetShowViewportSizeOnResize :: Handle ev -> POverlaySetShowViewportSizeOnResize -> IO ()
overlaySetShowViewportSizeOnResize handle params = sendReceiveCommand handle "Overlay.setShowViewportSizeOnResize" (Just params)


-- | Parameters of the 'overlaySetShowHinge' command.
data POverlaySetShowHinge = POverlaySetShowHinge {
  -- | hinge data, null means hideHinge
  pOverlaySetShowHingeHingeConfig :: Maybe OverlayHingeConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowHinge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowHinge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Overlay.setShowHinge' command.
--   Add a dual screen device hinge
--   Parameters: 'POverlaySetShowHinge'
overlaySetShowHinge :: Handle ev -> POverlaySetShowHinge -> IO ()
overlaySetShowHinge handle params = sendReceiveCommand handle "Overlay.setShowHinge" (Just params)


-- | Parameters of the 'overlaySetShowIsolatedElements' command.
data POverlaySetShowIsolatedElements = POverlaySetShowIsolatedElements {
  -- | An array of node identifiers and descriptors for the highlight appearance.
  pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs :: [OverlayIsolatedElementHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowIsolatedElements  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowIsolatedElements where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


-- | Function for the 'Overlay.setShowIsolatedElements' command.
--   Show elements in isolation mode with overlays.
--   Parameters: 'POverlaySetShowIsolatedElements'
overlaySetShowIsolatedElements :: Handle ev -> POverlaySetShowIsolatedElements -> IO ()
overlaySetShowIsolatedElements handle params = sendReceiveCommand handle "Overlay.setShowIsolatedElements" (Just params)



