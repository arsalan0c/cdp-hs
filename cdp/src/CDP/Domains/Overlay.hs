{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



data OverlaySourceOrderConfig = OverlaySourceOrderConfig {
   overlaySourceOrderConfigParentOutlineColor :: DOMPageNetworkEmulationSecurity.DomRgba,
   overlaySourceOrderConfigChildOutlineColor :: DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlaySourceOrderConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  OverlaySourceOrderConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



data OverlayGridHighlightConfig = OverlayGridHighlightConfig {
   overlayGridHighlightConfigShowGridExtensionLines :: Maybe Bool,
   overlayGridHighlightConfigShowPositiveLineNumbers :: Maybe Bool,
   overlayGridHighlightConfigShowNegativeLineNumbers :: Maybe Bool,
   overlayGridHighlightConfigShowAreaNames :: Maybe Bool,
   overlayGridHighlightConfigShowLineNames :: Maybe Bool,
   overlayGridHighlightConfigShowTrackSizes :: Maybe Bool,
   overlayGridHighlightConfigGridBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigRowLineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigColumnLineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigGridBorderDash :: Maybe Bool,
   overlayGridHighlightConfigRowLineDash :: Maybe Bool,
   overlayGridHighlightConfigColumnLineDash :: Maybe Bool,
   overlayGridHighlightConfigRowGapColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigRowHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigColumnGapColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigColumnHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigAreaBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayGridHighlightConfigGridBackgroundColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayGridHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }



data OverlayFlexContainerHighlightConfig = OverlayFlexContainerHighlightConfig {
   overlayFlexContainerHighlightConfigContainerBorder :: Maybe OverlayLineStyle,
   overlayFlexContainerHighlightConfigLineSeparator :: Maybe OverlayLineStyle,
   overlayFlexContainerHighlightConfigItemSeparator :: Maybe OverlayLineStyle,
   overlayFlexContainerHighlightConfigMainDistributedSpace :: Maybe OverlayBoxStyle,
   overlayFlexContainerHighlightConfigCrossDistributedSpace :: Maybe OverlayBoxStyle,
   overlayFlexContainerHighlightConfigRowGapSpace :: Maybe OverlayBoxStyle,
   overlayFlexContainerHighlightConfigColumnGapSpace :: Maybe OverlayBoxStyle,
   overlayFlexContainerHighlightConfigCrossAlignment :: Maybe OverlayLineStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



data OverlayFlexItemHighlightConfig = OverlayFlexItemHighlightConfig {
   overlayFlexItemHighlightConfigBaseSizeBox :: Maybe OverlayBoxStyle,
   overlayFlexItemHighlightConfigBaseSizeBorder :: Maybe OverlayLineStyle,
   overlayFlexItemHighlightConfigFlexibilityArrow :: Maybe OverlayLineStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexItemHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexItemHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }


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
   overlayLineStyleColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayLineStylePattern :: OverlayLineStylePattern
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayLineStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  OverlayLineStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data OverlayBoxStyle = OverlayBoxStyle {
   overlayBoxStyleFillColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayBoxStyleHatchColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayBoxStyle  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  OverlayBoxStyle where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


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



data OverlayHighlightConfig = OverlayHighlightConfig {
   overlayHighlightConfigShowInfo :: Maybe Bool,
   overlayHighlightConfigShowStyles :: Maybe Bool,
   overlayHighlightConfigShowRulers :: Maybe Bool,
   overlayHighlightConfigShowAccessibilityInfo :: Maybe Bool,
   overlayHighlightConfigShowExtensionLines :: Maybe Bool,
   overlayHighlightConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigBorderColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigEventTargetColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigShapeColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigShapeMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigCssGridColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHighlightConfigColorFormat :: Maybe OverlayColorFormat,
   overlayHighlightConfigGridHighlightConfig :: Maybe OverlayGridHighlightConfig,
   overlayHighlightConfigFlexContainerHighlightConfig :: Maybe OverlayFlexContainerHighlightConfig,
   overlayHighlightConfigFlexItemHighlightConfig :: Maybe OverlayFlexItemHighlightConfig,
   overlayHighlightConfigContrastAlgorithm :: Maybe OverlayContrastAlgorithm,
   overlayHighlightConfigContainerQueryContainerHighlightConfig :: Maybe OverlayContainerQueryContainerHighlightConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  OverlayHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


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



data OverlayGridNodeHighlightConfig = OverlayGridNodeHighlightConfig {
   overlayGridNodeHighlightConfigGridHighlightConfig :: OverlayGridHighlightConfig,
   overlayGridNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayGridNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayGridNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data OverlayFlexNodeHighlightConfig = OverlayFlexNodeHighlightConfig {
   overlayFlexNodeHighlightConfigFlexContainerHighlightConfig :: OverlayFlexContainerHighlightConfig,
   overlayFlexNodeHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayFlexNodeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  OverlayFlexNodeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data OverlayScrollSnapContainerHighlightConfig = OverlayScrollSnapContainerHighlightConfig {
   overlayScrollSnapContainerHighlightConfigSnapportBorder :: Maybe OverlayLineStyle,
   overlayScrollSnapContainerHighlightConfigSnapAreaBorder :: Maybe OverlayLineStyle,
   overlayScrollSnapContainerHighlightConfigScrollMarginColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayScrollSnapContainerHighlightConfigScrollPaddingColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 41 }



data OverlayScrollSnapHighlightConfig = OverlayScrollSnapHighlightConfig {
   overlayScrollSnapHighlightConfigScrollSnapContainerHighlightConfig :: OverlayScrollSnapContainerHighlightConfig,
   overlayScrollSnapHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScrollSnapHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  OverlayScrollSnapHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }



data OverlayHingeConfig = OverlayHingeConfig {
   overlayHingeConfigRect :: DOMPageNetworkEmulationSecurity.DomRect,
   overlayHingeConfigContentColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayHingeConfigOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayHingeConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  OverlayHingeConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data OverlayContainerQueryHighlightConfig = OverlayContainerQueryHighlightConfig {
   overlayContainerQueryHighlightConfigContainerQueryContainerHighlightConfig :: OverlayContainerQueryContainerHighlightConfig,
   overlayContainerQueryHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }



data OverlayContainerQueryContainerHighlightConfig = OverlayContainerQueryContainerHighlightConfig {
   overlayContainerQueryContainerHighlightConfigContainerBorder :: Maybe OverlayLineStyle,
   overlayContainerQueryContainerHighlightConfigDescendantBorder :: Maybe OverlayLineStyle
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayContainerQueryContainerHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  OverlayContainerQueryContainerHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }



data OverlayIsolatedElementHighlightConfig = OverlayIsolatedElementHighlightConfig {
   overlayIsolatedElementHighlightConfigIsolationModeHighlightConfig :: OverlayIsolationModeHighlightConfig,
   overlayIsolatedElementHighlightConfigNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolatedElementHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolatedElementHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }



data OverlayIsolationModeHighlightConfig = OverlayIsolationModeHighlightConfig {
   overlayIsolationModeHighlightConfigResizerColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayIsolationModeHighlightConfigResizerHandleColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   overlayIsolationModeHighlightConfigMaskColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayIsolationModeHighlightConfig  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  OverlayIsolationModeHighlightConfig where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


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





data OverlayInspectNodeRequested = OverlayInspectNodeRequested {
   overlayInspectNodeRequestedBackendNodeId :: DOMPageNetworkEmulationSecurity.DomBackendNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayInspectNodeRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  OverlayInspectNodeRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



data OverlayNodeHighlightRequested = OverlayNodeHighlightRequested {
   overlayNodeHighlightRequestedNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayNodeHighlightRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  OverlayNodeHighlightRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }



data OverlayScreenshotRequested = OverlayScreenshotRequested {
   overlayScreenshotRequestedViewport :: DOMPageNetworkEmulationSecurity.PageViewport
} deriving (Generic, Eq, Show, Read)
instance ToJSON OverlayScreenshotRequested  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  OverlayScreenshotRequested where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


data OverlayInspectModeCanceled = OverlayInspectModeCanceled
   deriving (Eq, Show, Read)
instance FromJSON OverlayInspectModeCanceled where
   parseJSON = A.withText  "OverlayInspectModeCanceled"  $ \v -> do
      case v of
         "OverlayInspectModeCanceled" -> pure OverlayInspectModeCanceled
         _ -> fail "failed to parse OverlayInspectModeCanceled"




overlayDisable :: Handle ev -> IO (Maybe Error)
overlayDisable handle = sendReceiveCommand handle "Overlay.disable" (Nothing :: Maybe ())


overlayEnable :: Handle ev -> IO (Maybe Error)
overlayEnable handle = sendReceiveCommand handle "Overlay.enable" (Nothing :: Maybe ())



data POverlayGetHighlightObjectForTest = POverlayGetHighlightObjectForTest {
   pOverlayGetHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId,
   pOverlayGetHighlightObjectForTestIncludeDistance :: Maybe Bool,
   pOverlayGetHighlightObjectForTestIncludeStyle :: Maybe Bool,
   pOverlayGetHighlightObjectForTestColorFormat :: Maybe OverlayColorFormat,
   pOverlayGetHighlightObjectForTestShowAccessibilityInfo :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


overlayGetHighlightObjectForTest :: Handle ev -> POverlayGetHighlightObjectForTest -> IO (Either Error OverlayGetHighlightObjectForTest)
overlayGetHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getHighlightObjectForTest" (Just params)

data OverlayGetHighlightObjectForTest = OverlayGetHighlightObjectForTest {
   overlayGetHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }

instance Command OverlayGetHighlightObjectForTest where
   commandName _ = "Overlay.getHighlightObjectForTest"




data POverlayGetGridHighlightObjectsForTest = POverlayGetGridHighlightObjectsForTest {
   pOverlayGetGridHighlightObjectsForTestNodeIds :: [DOMPageNetworkEmulationSecurity.DomNodeId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetGridHighlightObjectsForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 , A.omitNothingFields = True}

instance FromJSON  POverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 38 }


overlayGetGridHighlightObjectsForTest :: Handle ev -> POverlayGetGridHighlightObjectsForTest -> IO (Either Error OverlayGetGridHighlightObjectsForTest)
overlayGetGridHighlightObjectsForTest handle params = sendReceiveCommandResult handle "Overlay.getGridHighlightObjectsForTest" (Just params)

data OverlayGetGridHighlightObjectsForTest = OverlayGetGridHighlightObjectsForTest {
   overlayGetGridHighlightObjectsForTestHighlights :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetGridHighlightObjectsForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }

instance Command OverlayGetGridHighlightObjectsForTest where
   commandName _ = "Overlay.getGridHighlightObjectsForTest"




data POverlayGetSourceOrderHighlightObjectForTest = POverlayGetSourceOrderHighlightObjectForTest {
   pOverlayGetSourceOrderHighlightObjectForTestNodeId :: DOMPageNetworkEmulationSecurity.DomNodeId
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayGetSourceOrderHighlightObjectForTest  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 , A.omitNothingFields = True}

instance FromJSON  POverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 44 }


overlayGetSourceOrderHighlightObjectForTest :: Handle ev -> POverlayGetSourceOrderHighlightObjectForTest -> IO (Either Error OverlayGetSourceOrderHighlightObjectForTest)
overlayGetSourceOrderHighlightObjectForTest handle params = sendReceiveCommandResult handle "Overlay.getSourceOrderHighlightObjectForTest" (Just params)

data OverlayGetSourceOrderHighlightObjectForTest = OverlayGetSourceOrderHighlightObjectForTest {
   overlayGetSourceOrderHighlightObjectForTestHighlight :: [(String, String)]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  OverlayGetSourceOrderHighlightObjectForTest where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 43 }

instance Command OverlayGetSourceOrderHighlightObjectForTest where
   commandName _ = "Overlay.getSourceOrderHighlightObjectForTest"



overlayHideHighlight :: Handle ev -> IO (Maybe Error)
overlayHideHighlight handle = sendReceiveCommand handle "Overlay.hideHighlight" (Nothing :: Maybe ())



data POverlayHighlightNode = POverlayHighlightNode {
   pOverlayHighlightNodeHighlightConfig :: OverlayHighlightConfig,
   pOverlayHighlightNodeNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
   pOverlayHighlightNodeBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   pOverlayHighlightNodeObjectId :: Maybe Runtime.RuntimeRemoteObjectId,
   pOverlayHighlightNodeSelector :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightNode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightNode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


overlayHighlightNode :: Handle ev -> POverlayHighlightNode -> IO (Maybe Error)
overlayHighlightNode handle params = sendReceiveCommand handle "Overlay.highlightNode" (Just params)



data POverlayHighlightQuad = POverlayHighlightQuad {
   pOverlayHighlightQuadQuad :: DOMPageNetworkEmulationSecurity.DomQuad,
   pOverlayHighlightQuadColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   pOverlayHighlightQuadOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightQuad  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightQuad where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


overlayHighlightQuad :: Handle ev -> POverlayHighlightQuad -> IO (Maybe Error)
overlayHighlightQuad handle params = sendReceiveCommand handle "Overlay.highlightQuad" (Just params)



data POverlayHighlightRect = POverlayHighlightRect {
   pOverlayHighlightRectX :: Int,
   pOverlayHighlightRectY :: Int,
   pOverlayHighlightRectWidth :: Int,
   pOverlayHighlightRectHeight :: Int,
   pOverlayHighlightRectColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba,
   pOverlayHighlightRectOutlineColor :: Maybe DOMPageNetworkEmulationSecurity.DomRgba
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


overlayHighlightRect :: Handle ev -> POverlayHighlightRect -> IO (Maybe Error)
overlayHighlightRect handle params = sendReceiveCommand handle "Overlay.highlightRect" (Just params)



data POverlayHighlightSourceOrder = POverlayHighlightSourceOrder {
   pOverlayHighlightSourceOrderSourceOrderConfig :: OverlaySourceOrderConfig,
   pOverlayHighlightSourceOrderNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomNodeId,
   pOverlayHighlightSourceOrderBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   pOverlayHighlightSourceOrderObjectId :: Maybe Runtime.RuntimeRemoteObjectId
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlayHighlightSourceOrder  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  POverlayHighlightSourceOrder where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


overlayHighlightSourceOrder :: Handle ev -> POverlayHighlightSourceOrder -> IO (Maybe Error)
overlayHighlightSourceOrder handle params = sendReceiveCommand handle "Overlay.highlightSourceOrder" (Just params)



data POverlaySetInspectMode = POverlaySetInspectMode {
   pOverlaySetInspectModeMode :: OverlayInspectMode,
   pOverlaySetInspectModeHighlightConfig :: Maybe OverlayHighlightConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetInspectMode  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  POverlaySetInspectMode where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


overlaySetInspectMode :: Handle ev -> POverlaySetInspectMode -> IO (Maybe Error)
overlaySetInspectMode handle params = sendReceiveCommand handle "Overlay.setInspectMode" (Just params)



data POverlaySetShowAdHighlights = POverlaySetShowAdHighlights {
   pOverlaySetShowAdHighlightsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowAdHighlights  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowAdHighlights where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


overlaySetShowAdHighlights :: Handle ev -> POverlaySetShowAdHighlights -> IO (Maybe Error)
overlaySetShowAdHighlights handle params = sendReceiveCommand handle "Overlay.setShowAdHighlights" (Just params)



data POverlaySetPausedInDebuggerMessage = POverlaySetPausedInDebuggerMessage {
   pOverlaySetPausedInDebuggerMessageMessage :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetPausedInDebuggerMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 , A.omitNothingFields = True}

instance FromJSON  POverlaySetPausedInDebuggerMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 34 }


overlaySetPausedInDebuggerMessage :: Handle ev -> POverlaySetPausedInDebuggerMessage -> IO (Maybe Error)
overlaySetPausedInDebuggerMessage handle params = sendReceiveCommand handle "Overlay.setPausedInDebuggerMessage" (Just params)



data POverlaySetShowDebugBorders = POverlaySetShowDebugBorders {
   pOverlaySetShowDebugBordersShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowDebugBorders  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowDebugBorders where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


overlaySetShowDebugBorders :: Handle ev -> POverlaySetShowDebugBorders -> IO (Maybe Error)
overlaySetShowDebugBorders handle params = sendReceiveCommand handle "Overlay.setShowDebugBorders" (Just params)



data POverlaySetShowFpsCounter = POverlaySetShowFpsCounter {
   pOverlaySetShowFpsCounterShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFpsCounter  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFpsCounter where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


overlaySetShowFpsCounter :: Handle ev -> POverlaySetShowFpsCounter -> IO (Maybe Error)
overlaySetShowFpsCounter handle params = sendReceiveCommand handle "Overlay.setShowFPSCounter" (Just params)



data POverlaySetShowGridOverlays = POverlaySetShowGridOverlays {
   pOverlaySetShowGridOverlaysGridNodeHighlightConfigs :: [OverlayGridNodeHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowGridOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowGridOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


overlaySetShowGridOverlays :: Handle ev -> POverlaySetShowGridOverlays -> IO (Maybe Error)
overlaySetShowGridOverlays handle params = sendReceiveCommand handle "Overlay.setShowGridOverlays" (Just params)



data POverlaySetShowFlexOverlays = POverlaySetShowFlexOverlays {
   pOverlaySetShowFlexOverlaysFlexNodeHighlightConfigs :: [OverlayFlexNodeHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowFlexOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowFlexOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


overlaySetShowFlexOverlays :: Handle ev -> POverlaySetShowFlexOverlays -> IO (Maybe Error)
overlaySetShowFlexOverlays handle params = sendReceiveCommand handle "Overlay.setShowFlexOverlays" (Just params)



data POverlaySetShowScrollSnapOverlays = POverlaySetShowScrollSnapOverlays {
   pOverlaySetShowScrollSnapOverlaysScrollSnapHighlightConfigs :: [OverlayScrollSnapHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollSnapOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollSnapOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


overlaySetShowScrollSnapOverlays :: Handle ev -> POverlaySetShowScrollSnapOverlays -> IO (Maybe Error)
overlaySetShowScrollSnapOverlays handle params = sendReceiveCommand handle "Overlay.setShowScrollSnapOverlays" (Just params)



data POverlaySetShowContainerQueryOverlays = POverlaySetShowContainerQueryOverlays {
   pOverlaySetShowContainerQueryOverlaysContainerQueryHighlightConfigs :: [OverlayContainerQueryHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowContainerQueryOverlays  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowContainerQueryOverlays where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 37 }


overlaySetShowContainerQueryOverlays :: Handle ev -> POverlaySetShowContainerQueryOverlays -> IO (Maybe Error)
overlaySetShowContainerQueryOverlays handle params = sendReceiveCommand handle "Overlay.setShowContainerQueryOverlays" (Just params)



data POverlaySetShowPaintRects = POverlaySetShowPaintRects {
   pOverlaySetShowPaintRectsResult :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowPaintRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowPaintRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


overlaySetShowPaintRects :: Handle ev -> POverlaySetShowPaintRects -> IO (Maybe Error)
overlaySetShowPaintRects handle params = sendReceiveCommand handle "Overlay.setShowPaintRects" (Just params)



data POverlaySetShowLayoutShiftRegions = POverlaySetShowLayoutShiftRegions {
   pOverlaySetShowLayoutShiftRegionsResult :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowLayoutShiftRegions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowLayoutShiftRegions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }


overlaySetShowLayoutShiftRegions :: Handle ev -> POverlaySetShowLayoutShiftRegions -> IO (Maybe Error)
overlaySetShowLayoutShiftRegions handle params = sendReceiveCommand handle "Overlay.setShowLayoutShiftRegions" (Just params)



data POverlaySetShowScrollBottleneckRects = POverlaySetShowScrollBottleneckRects {
   pOverlaySetShowScrollBottleneckRectsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowScrollBottleneckRects  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowScrollBottleneckRects where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 36 }


overlaySetShowScrollBottleneckRects :: Handle ev -> POverlaySetShowScrollBottleneckRects -> IO (Maybe Error)
overlaySetShowScrollBottleneckRects handle params = sendReceiveCommand handle "Overlay.setShowScrollBottleneckRects" (Just params)



data POverlaySetShowWebVitals = POverlaySetShowWebVitals {
   pOverlaySetShowWebVitalsShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowWebVitals  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowWebVitals where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


overlaySetShowWebVitals :: Handle ev -> POverlaySetShowWebVitals -> IO (Maybe Error)
overlaySetShowWebVitals handle params = sendReceiveCommand handle "Overlay.setShowWebVitals" (Just params)



data POverlaySetShowViewportSizeOnResize = POverlaySetShowViewportSizeOnResize {
   pOverlaySetShowViewportSizeOnResizeShow :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowViewportSizeOnResize  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowViewportSizeOnResize where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


overlaySetShowViewportSizeOnResize :: Handle ev -> POverlaySetShowViewportSizeOnResize -> IO (Maybe Error)
overlaySetShowViewportSizeOnResize handle params = sendReceiveCommand handle "Overlay.setShowViewportSizeOnResize" (Just params)



data POverlaySetShowHinge = POverlaySetShowHinge {
   pOverlaySetShowHingeHingeConfig :: Maybe OverlayHingeConfig
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowHinge  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowHinge where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


overlaySetShowHinge :: Handle ev -> POverlaySetShowHinge -> IO (Maybe Error)
overlaySetShowHinge handle params = sendReceiveCommand handle "Overlay.setShowHinge" (Just params)



data POverlaySetShowIsolatedElements = POverlaySetShowIsolatedElements {
   pOverlaySetShowIsolatedElementsIsolatedElementHighlightConfigs :: [OverlayIsolatedElementHighlightConfig]
} deriving (Generic, Eq, Show, Read)
instance ToJSON POverlaySetShowIsolatedElements  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  POverlaySetShowIsolatedElements where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


overlaySetShowIsolatedElements :: Handle ev -> POverlaySetShowIsolatedElements -> IO (Maybe Error)
overlaySetShowIsolatedElements handle params = sendReceiveCommand handle "Overlay.setShowIsolatedElements" (Just params)



