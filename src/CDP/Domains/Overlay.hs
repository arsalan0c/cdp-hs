{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Runtime


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.Runtime as Runtime


-- | Type 'Overlay.SourceOrderConfig'.
--   Configuration data for drawing the source order of an elements children.
data OverlaySourceOrderConfig = OverlaySourceOrderConfig {
  -- | the color to outline the givent element in.
  overlaySourceOrderConfigParentOutlineColor :: DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | the color to outline the child elements in.
  overlaySourceOrderConfigChildOutlineColor :: DOMPageNetworkEmulationSecurity.DOMRGBA
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
  overlayLineStyleColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
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
  overlayBoxStyleFillColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The hatching color for the box (default: transparent)
  overlayBoxStyleHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
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
  overlayGridNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
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
  overlayFlexNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
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
  overlayScrollSnapContainerHighlightConfigScrollMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The padding highlight fill color (default: transparent).
  overlayScrollSnapContainerHighlightConfigScrollPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
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
  overlayScrollSnapHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



-- | Type 'Overlay.HingeConfig'.
--   Configuration for dual screen hinge
data OverlayHingeConfig = OverlayHingeConfig {
  -- | A rectangle represent hinge
  overlayHingeConfigRect :: DOMPageNetworkEmulationSecurity.DOMRect,
  -- | The content box highlight fill color (default: a dark color).
  overlayHingeConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The content box highlight outline color (default: transparent).
  overlayHingeConfigOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
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
  overlayContainerQueryHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
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
  overlayIsolatedElementHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolatedElementHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolatedElementHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



-- | Type 'Overlay.IsolationModeHighlightConfig'.
data OverlayIsolationModeHighlightConfig = OverlayIsolationModeHighlightConfig {
  -- | The fill color of the resizers (default: transparent).
  overlayIsolationModeHighlightConfigResizerColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The fill color for resizer handles (default: transparent).
  overlayIsolationModeHighlightConfigResizerHandleColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The fill color for the mask covering non-isolated elements (default: transparent).
  overlayIsolationModeHighlightConfigMaskColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolationModeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolationModeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'Overlay.InspectMode'.
data OverlayInspectMode = OverlayInspectModeSearchForNode | OverlayInspectModeSearchForUAShadowDOM | OverlayInspectModeCaptureAreaScreenshot | OverlayInspectModeShowDistances | OverlayInspectModeNone
   deriving (Ord, Eq, Show, Read)
instance FromJSON OverlayInspectMode where
   parseJSON = A.withText  "OverlayInspectMode"  $ \v -> do
      case v of
         "searchForNode" -> pure OverlayInspectModeSearchForNode
         "searchForUAShadowDOM" -> pure OverlayInspectModeSearchForUAShadowDOM
         "captureAreaScreenshot" -> pure OverlayInspectModeCaptureAreaScreenshot
         "showDistances" -> pure OverlayInspectModeShowDistances
         "none" -> pure OverlayInspectModeNone
         _ -> fail "failed to parse OverlayInspectMode"

instance ToJSON OverlayInspectMode where
   toJSON v = A.String $
      case v of
         OverlayInspectModeSearchForNode -> "searchForNode"
         OverlayInspectModeSearchForUAShadowDOM -> "searchForUAShadowDOM"
         OverlayInspectModeCaptureAreaScreenshot -> "captureAreaScreenshot"
         OverlayInspectModeShowDistances -> "showDistances"
         OverlayInspectModeNone -> "none"





-- | Type of the 'Overlay.inspectNodeRequested' event.
data OverlayInspectNodeRequested = OverlayInspectNodeRequested {
  -- | Id of the node to inspect.
  overlayInspectNodeRequestedBackendNodeId :: DOMPageNetworkEmulationSecurity.DOMBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayInspectNodeRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  OverlayInspectNodeRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


instance Event OverlayInspectNodeRequested where
    eventName _ = "Overlay.inspectNodeRequested"

-- | Type of the 'Overlay.nodeHighlightRequested' event.
data OverlayNodeHighlightRequested = OverlayNodeHighlightRequested {
  overlayNodeHighlightRequestedNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayNodeHighlightRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  OverlayNodeHighlightRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


instance Event OverlayNodeHighlightRequested where
    eventName _ = "Overlay.nodeHighlightRequested"

-- | Type of the 'Overlay.screenshotRequested' event.
data OverlayScreenshotRequested = OverlayScreenshotRequested {
  -- | Viewport to capture, in device independent pixels (dip).
  overlayScreenshotRequestedViewport :: DOMPageNetworkEmulationSecurity.PageViewport
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScreenshotRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayScreenshotRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Event OverlayScreenshotRequested where
    eventName _ = "Overlay.screenshotRequested"

-- | Type of the 'Overlay.inspectModeCanceled' event.
data OverlayInspectModeCanceled = OverlayInspectModeCanceled
   deriving (Eq, Show, Read)
instance FromJSON OverlayInspectModeCanceled where
   parseJSON = A.withText  "OverlayInspectModeCanceled"  $ \v -> do
      case v of
         "OverlayInspectModeCanceled" -> pure OverlayInspectModeCanceled
         _ -> fail "failed to parse OverlayInspectModeCanceled"


instance Event OverlayInspectModeCanceled where
    eventName _ = "Overlay.inspectModeCanceled"



-- | Parameters of the 'overlayDisable' command.
data POverlayDisable = POverlayDisable
instance ToJSON POverlayDisable where toJSON _ = A.Null

-- | Function for the 'Overlay.disable' command.
--   Disables domain notifications.
overlayDisable :: Handle -> IO ()
overlayDisable handle = sendReceiveCommand handle POverlayDisable

instance Command POverlayDisable where
    type CommandResponse POverlayDisable = NoResponse
    commandName _ = "Overlay.disable"


-- | Parameters of the 'overlayEnable' command.
data POverlayEnable = POverlayEnable
instance ToJSON POverlayEnable where toJSON _ = A.Null

-- | Function for the 'Overlay.enable' command.
--   Enables domain notifications.
overlayEnable :: Handle -> IO ()
overlayEnable handle = sendReceiveCommand handle POverlayEnable

instance Command POverlayEnable where
    type CommandResponse POverlayEnable = NoResponse
    commandName _ = "Overlay.enable"


-- | Parameters of the 'overlayGetHighlightObjectForTest' command.
data POverlayGetHighlightObjectForTest = POverlayGetHighlightObjectForTest {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


-- | Function for the 'Overlay.getHighlightObjectForTest' command.
--   For testing.
--   Returns: 'POverlayGetHighlightObjectForTest'
--   Returns: 'OverlayGetHighlightObjectForTest'
overlayGetHighlightObjectForTest :: Handle -> POverlayGetHighlightObjectForTest -> IO OverlayGetHighlightObjectForTest
overlayGetHighlightObjectForTest handle params = sendReceiveCommandResult handle params

-- | Return type of the 'overlayGetHighlightObjectForTest' command.
data OverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest {
  -- | Highlight data for the node.
  overlayGetHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance Command POverlayGetHighlightObjectForTest where
    type CommandResponse POverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest
    commandName _ = "Overlay.getHighlightObjectForTest"


-- | Parameters of the 'overlayGetGridHighlightObjectsForTest' command.
data POverlayGetGridHighlightObjectsForTest = POverlayGetGridHighlightObjectsForTest {
  -- | Ids of the node to get highlight object for.
  pOverlayGetGridHighlightObjectsForTestNodeIds :: [DOMPageNetworkEmulationSecurity.DOMNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetGridHighlightObjectsForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  POverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


-- | Function for the 'Overlay.getGridHighlightObjectsForTest' command.
--   For Persistent Grid testing.
--   Returns: 'POverlayGetGridHighlightObjectsForTest'
--   Returns: 'OverlayGetGridHighlightObjectsForTest'
overlayGetGridHighlightObjectsForTest :: Handle -> POverlayGetGridHighlightObjectsForTest -> IO OverlayGetGridHighlightObjectsForTest
overlayGetGridHighlightObjectsForTest handle params = sendReceiveCommandResult handle params

-- | Return type of the 'overlayGetGridHighlightObjectsForTest' command.
data OverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest {
  -- | Grid Highlight data for the node ids provided.
  overlayGetGridHighlightObjectsForTestHighlights :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command POverlayGetGridHighlightObjectsForTest where
    type CommandResponse POverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest
    commandName _ = "Overlay.getGridHighlightObjectsForTest"


-- | Parameters of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data POverlayGetSourceOrderHighlightObjectForTest = POverlayGetSourceOrderHighlightObjectForTest {
  -- | Id of the node to highlight.
  pOverlayGetSourceOrderHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DOMNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetSourceOrderHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  POverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }


-- | Function for the 'Overlay.getSourceOrderHighlightObjectForTest' command.
--   For Source Order Viewer testing.
--   Returns: 'POverlayGetSourceOrderHighlightObjectForTest'
--   Returns: 'OverlayGetSourceOrderHighlightObjectForTest'
overlayGetSourceOrderHighlightObjectForTest :: Handle -> POverlayGetSourceOrderHighlightObjectForTest -> IO OverlayGetSourceOrderHighlightObjectForTest
overlayGetSourceOrderHighlightObjectForTest handle params = sendReceiveCommandResult handle params

-- | Return type of the 'overlayGetSourceOrderHighlightObjectForTest' command.
data OverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest {
  -- | Source order highlight data for the node id provided.
  overlayGetSourceOrderHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }

instance Command POverlayGetSourceOrderHighlightObjectForTest where
    type CommandResponse POverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest
    commandName _ = "Overlay.getSourceOrderHighlightObjectForTest"


-- | Parameters of the 'overlayHideHighlight' command.
data POverlayHideHighlight = POverlayHideHighlight
instance ToJSON POverlayHideHighlight where toJSON _ = A.Null

-- | Function for the 'Overlay.hideHighlight' command.
--   Hides any highlight.
overlayHideHighlight :: Handle -> IO ()
overlayHideHighlight handle = sendReceiveCommand handle POverlayHideHighlight

instance Command POverlayHideHighlight where
    type CommandResponse POverlayHideHighlight = NoResponse
    commandName _ = "Overlay.hideHighlight"


-- | Parameters of the 'overlayHighlightNode' command.
data POverlayHighlightNode = POverlayHighlightNode {
  -- | A descriptor for the highlight appearance.
  pOverlayHighlightNodeHighlightConfig :: OverlayHighlightConfig,
  -- | Identifier of the node to highlight.
  pOverlayHighlightNodeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Identifier of the backend node to highlight.
  pOverlayHighlightNodeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
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
--   Returns: 'POverlayHighlightNode'
overlayHighlightNode :: Handle -> POverlayHighlightNode -> IO ()
overlayHighlightNode handle params = sendReceiveCommand handle params

instance Command POverlayHighlightNode where
    type CommandResponse POverlayHighlightNode = NoResponse
    commandName _ = "Overlay.highlightNode"


-- | Parameters of the 'overlayHighlightQuad' command.
data POverlayHighlightQuad = POverlayHighlightQuad {
  -- | Quad to highlight
  pOverlayHighlightQuadQuad :: DOMPageNetworkEmulationSecurity.DOMQuad,
  -- | The highlight fill color (default: transparent).
  pOverlayHighlightQuadColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The highlight outline color (default: transparent).
  pOverlayHighlightQuadOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightQuad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightQuad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Overlay.highlightQuad' command.
--   Highlights given quad. Coordinates are absolute with respect to the main frame viewport.
--   Returns: 'POverlayHighlightQuad'
overlayHighlightQuad :: Handle -> POverlayHighlightQuad -> IO ()
overlayHighlightQuad handle params = sendReceiveCommand handle params

instance Command POverlayHighlightQuad where
    type CommandResponse POverlayHighlightQuad = NoResponse
    commandName _ = "Overlay.highlightQuad"


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
  pOverlayHighlightRectColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA,
  -- | The highlight outline color (default: transparent).
  pOverlayHighlightRectOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DOMRGBA
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Overlay.highlightRect' command.
--   Highlights given rectangle. Coordinates are absolute with respect to the main frame viewport.
--   Returns: 'POverlayHighlightRect'
overlayHighlightRect :: Handle -> POverlayHighlightRect -> IO ()
overlayHighlightRect handle params = sendReceiveCommand handle params

instance Command POverlayHighlightRect where
    type CommandResponse POverlayHighlightRect = NoResponse
    commandName _ = "Overlay.highlightRect"


-- | Parameters of the 'overlayHighlightSourceOrder' command.
data POverlayHighlightSourceOrder = POverlayHighlightSourceOrder {
  -- | A descriptor for the appearance of the overlay drawing.
  pOverlayHighlightSourceOrderSourceOrderConfig :: OverlaySourceOrderConfig,
  -- | Identifier of the node to highlight.
  pOverlayHighlightSourceOrderNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMNodeId,
  -- | Identifier of the backend node to highlight.
  pOverlayHighlightSourceOrderBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
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
--   Returns: 'POverlayHighlightSourceOrder'
overlayHighlightSourceOrder :: Handle -> POverlayHighlightSourceOrder -> IO ()
overlayHighlightSourceOrder handle params = sendReceiveCommand handle params

instance Command POverlayHighlightSourceOrder where
    type CommandResponse POverlayHighlightSourceOrder = NoResponse
    commandName _ = "Overlay.highlightSourceOrder"


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
--   Returns: 'POverlaySetInspectMode'
overlaySetInspectMode :: Handle -> POverlaySetInspectMode -> IO ()
overlaySetInspectMode handle params = sendReceiveCommand handle params

instance Command POverlaySetInspectMode where
    type CommandResponse POverlaySetInspectMode = NoResponse
    commandName _ = "Overlay.setInspectMode"


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
--   Returns: 'POverlaySetShowAdHighlights'
overlaySetShowAdHighlights :: Handle -> POverlaySetShowAdHighlights -> IO ()
overlaySetShowAdHighlights handle params = sendReceiveCommand handle params

instance Command POverlaySetShowAdHighlights where
    type CommandResponse POverlaySetShowAdHighlights = NoResponse
    commandName _ = "Overlay.setShowAdHighlights"


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
--   Returns: 'POverlaySetPausedInDebuggerMessage'
overlaySetPausedInDebuggerMessage :: Handle -> POverlaySetPausedInDebuggerMessage -> IO ()
overlaySetPausedInDebuggerMessage handle params = sendReceiveCommand handle params

instance Command POverlaySetPausedInDebuggerMessage where
    type CommandResponse POverlaySetPausedInDebuggerMessage = NoResponse
    commandName _ = "Overlay.setPausedInDebuggerMessage"


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
--   Returns: 'POverlaySetShowDebugBorders'
overlaySetShowDebugBorders :: Handle -> POverlaySetShowDebugBorders -> IO ()
overlaySetShowDebugBorders handle params = sendReceiveCommand handle params

instance Command POverlaySetShowDebugBorders where
    type CommandResponse POverlaySetShowDebugBorders = NoResponse
    commandName _ = "Overlay.setShowDebugBorders"


-- | Parameters of the 'overlaySetShowFPSCounter' command.
data POverlaySetShowFPSCounter = POverlaySetShowFPSCounter {
  -- | True for showing the FPS counter
  pOverlaySetShowFPSCounterShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFPSCounter  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFPSCounter where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Overlay.setShowFPSCounter' command.
--   Requests that backend shows the FPS counter
--   Returns: 'POverlaySetShowFPSCounter'
overlaySetShowFPSCounter :: Handle -> POverlaySetShowFPSCounter -> IO ()
overlaySetShowFPSCounter handle params = sendReceiveCommand handle params

instance Command POverlaySetShowFPSCounter where
    type CommandResponse POverlaySetShowFPSCounter = NoResponse
    commandName _ = "Overlay.setShowFPSCounter"


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
--   Returns: 'POverlaySetShowGridOverlays'
overlaySetShowGridOverlays :: Handle -> POverlaySetShowGridOverlays -> IO ()
overlaySetShowGridOverlays handle params = sendReceiveCommand handle params

instance Command POverlaySetShowGridOverlays where
    type CommandResponse POverlaySetShowGridOverlays = NoResponse
    commandName _ = "Overlay.setShowGridOverlays"


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
--   Returns: 'POverlaySetShowFlexOverlays'
overlaySetShowFlexOverlays :: Handle -> POverlaySetShowFlexOverlays -> IO ()
overlaySetShowFlexOverlays handle params = sendReceiveCommand handle params

instance Command POverlaySetShowFlexOverlays where
    type CommandResponse POverlaySetShowFlexOverlays = NoResponse
    commandName _ = "Overlay.setShowFlexOverlays"


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
--   Returns: 'POverlaySetShowScrollSnapOverlays'
overlaySetShowScrollSnapOverlays :: Handle -> POverlaySetShowScrollSnapOverlays -> IO ()
overlaySetShowScrollSnapOverlays handle params = sendReceiveCommand handle params

instance Command POverlaySetShowScrollSnapOverlays where
    type CommandResponse POverlaySetShowScrollSnapOverlays = NoResponse
    commandName _ = "Overlay.setShowScrollSnapOverlays"


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
--   Returns: 'POverlaySetShowContainerQueryOverlays'
overlaySetShowContainerQueryOverlays :: Handle -> POverlaySetShowContainerQueryOverlays -> IO ()
overlaySetShowContainerQueryOverlays handle params = sendReceiveCommand handle params

instance Command POverlaySetShowContainerQueryOverlays where
    type CommandResponse POverlaySetShowContainerQueryOverlays = NoResponse
    commandName _ = "Overlay.setShowContainerQueryOverlays"


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
--   Returns: 'POverlaySetShowPaintRects'
overlaySetShowPaintRects :: Handle -> POverlaySetShowPaintRects -> IO ()
overlaySetShowPaintRects handle params = sendReceiveCommand handle params

instance Command POverlaySetShowPaintRects where
    type CommandResponse POverlaySetShowPaintRects = NoResponse
    commandName _ = "Overlay.setShowPaintRects"


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
--   Returns: 'POverlaySetShowLayoutShiftRegions'
overlaySetShowLayoutShiftRegions :: Handle -> POverlaySetShowLayoutShiftRegions -> IO ()
overlaySetShowLayoutShiftRegions handle params = sendReceiveCommand handle params

instance Command POverlaySetShowLayoutShiftRegions where
    type CommandResponse POverlaySetShowLayoutShiftRegions = NoResponse
    commandName _ = "Overlay.setShowLayoutShiftRegions"


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
--   Returns: 'POverlaySetShowScrollBottleneckRects'
overlaySetShowScrollBottleneckRects :: Handle -> POverlaySetShowScrollBottleneckRects -> IO ()
overlaySetShowScrollBottleneckRects handle params = sendReceiveCommand handle params

instance Command POverlaySetShowScrollBottleneckRects where
    type CommandResponse POverlaySetShowScrollBottleneckRects = NoResponse
    commandName _ = "Overlay.setShowScrollBottleneckRects"


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
--   Returns: 'POverlaySetShowWebVitals'
overlaySetShowWebVitals :: Handle -> POverlaySetShowWebVitals -> IO ()
overlaySetShowWebVitals handle params = sendReceiveCommand handle params

instance Command POverlaySetShowWebVitals where
    type CommandResponse POverlaySetShowWebVitals = NoResponse
    commandName _ = "Overlay.setShowWebVitals"


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
--   Returns: 'POverlaySetShowViewportSizeOnResize'
overlaySetShowViewportSizeOnResize :: Handle -> POverlaySetShowViewportSizeOnResize -> IO ()
overlaySetShowViewportSizeOnResize handle params = sendReceiveCommand handle params

instance Command POverlaySetShowViewportSizeOnResize where
    type CommandResponse POverlaySetShowViewportSizeOnResize = NoResponse
    commandName _ = "Overlay.setShowViewportSizeOnResize"


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
--   Returns: 'POverlaySetShowHinge'
overlaySetShowHinge :: Handle -> POverlaySetShowHinge -> IO ()
overlaySetShowHinge handle params = sendReceiveCommand handle params

instance Command POverlaySetShowHinge where
    type CommandResponse POverlaySetShowHinge = NoResponse
    commandName _ = "Overlay.setShowHinge"


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
--   Returns: 'POverlaySetShowIsolatedElements'
overlaySetShowIsolatedElements :: Handle -> POverlaySetShowIsolatedElements -> IO ()
overlaySetShowIsolatedElements handle params = sendReceiveCommand handle params

instance Command POverlaySetShowIsolatedElements where
    type CommandResponse POverlaySetShowIsolatedElements = NoResponse
    commandName _ = "Overlay.setShowIsolatedElements"



