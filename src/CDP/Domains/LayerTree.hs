{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= LayerTree

-}


module CDP.Domains.LayerTree (module CDP.Domains.LayerTree) where

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


-- | Type 'LayerTree.LayerId'.
--   Unique Layer identifier.
type LayerTreeLayerId = String

-- | Type 'LayerTree.SnapshotId'.
--   Unique snapshot identifier.
type LayerTreeSnapshotId = String

-- | Type 'LayerTree.ScrollRect'.
--   Rectangle where scrolling happens on the main thread.
data LayerTreeScrollRectType = LayerTreeScrollRectTypeRepaintsOnScroll | LayerTreeScrollRectTypeTouchEventHandler | LayerTreeScrollRectTypeWheelEventHandler
  deriving (Ord, Eq, Show, Read)
instance FromJSON LayerTreeScrollRectType where
  parseJSON = A.withText "LayerTreeScrollRectType" $ \v -> case v of
    "RepaintsOnScroll" -> pure LayerTreeScrollRectTypeRepaintsOnScroll
    "TouchEventHandler" -> pure LayerTreeScrollRectTypeTouchEventHandler
    "WheelEventHandler" -> pure LayerTreeScrollRectTypeWheelEventHandler
    "_" -> fail "failed to parse LayerTreeScrollRectType"
instance ToJSON LayerTreeScrollRectType where
  toJSON v = A.String $ case v of
    LayerTreeScrollRectTypeRepaintsOnScroll -> "RepaintsOnScroll"
    LayerTreeScrollRectTypeTouchEventHandler -> "TouchEventHandler"
    LayerTreeScrollRectTypeWheelEventHandler -> "WheelEventHandler"
data LayerTreeScrollRect = LayerTreeScrollRect
  {
    -- | Rectangle itself.
    layerTreeScrollRectRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | Reason for rectangle to force scrolling on the main thread
    layerTreeScrollRectType :: LayerTreeScrollRectType
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeScrollRect where
  parseJSON = A.withObject "LayerTreeScrollRect" $ \o -> LayerTreeScrollRect
    <$> o A..: "rect"
    <*> o A..: "type"
instance ToJSON LayerTreeScrollRect where
  toJSON p = A.object $ catMaybes [
    ("rect" A..=) <$> Just (layerTreeScrollRectRect p),
    ("type" A..=) <$> Just (layerTreeScrollRectType p)
    ]

-- | Type 'LayerTree.StickyPositionConstraint'.
--   Sticky position constraints.
data LayerTreeStickyPositionConstraint = LayerTreeStickyPositionConstraint
  {
    -- | Layout rectangle of the sticky element before being shifted
    layerTreeStickyPositionConstraintStickyBoxRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | Layout rectangle of the containing block of the sticky element
    layerTreeStickyPositionConstraintContainingBlockRect :: DOMPageNetworkEmulationSecurity.DOMRect,
    -- | The nearest sticky layer that shifts the sticky box
    layerTreeStickyPositionConstraintNearestLayerShiftingStickyBox :: Maybe LayerTreeLayerId,
    -- | The nearest sticky layer that shifts the containing block
    layerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock :: Maybe LayerTreeLayerId
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeStickyPositionConstraint where
  parseJSON = A.withObject "LayerTreeStickyPositionConstraint" $ \o -> LayerTreeStickyPositionConstraint
    <$> o A..: "stickyBoxRect"
    <*> o A..: "containingBlockRect"
    <*> o A..:? "nearestLayerShiftingStickyBox"
    <*> o A..:? "nearestLayerShiftingContainingBlock"
instance ToJSON LayerTreeStickyPositionConstraint where
  toJSON p = A.object $ catMaybes [
    ("stickyBoxRect" A..=) <$> Just (layerTreeStickyPositionConstraintStickyBoxRect p),
    ("containingBlockRect" A..=) <$> Just (layerTreeStickyPositionConstraintContainingBlockRect p),
    ("nearestLayerShiftingStickyBox" A..=) <$> (layerTreeStickyPositionConstraintNearestLayerShiftingStickyBox p),
    ("nearestLayerShiftingContainingBlock" A..=) <$> (layerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock p)
    ]

-- | Type 'LayerTree.PictureTile'.
--   Serialized fragment of layer picture along with its offset within the layer.
data LayerTreePictureTile = LayerTreePictureTile
  {
    -- | Offset from owning layer left boundary
    layerTreePictureTileX :: Double,
    -- | Offset from owning layer top boundary
    layerTreePictureTileY :: Double,
    -- | Base64-encoded snapshot data. (Encoded as a base64 string when passed over JSON)
    layerTreePictureTilePicture :: String
  }
  deriving (Eq, Show)
instance FromJSON LayerTreePictureTile where
  parseJSON = A.withObject "LayerTreePictureTile" $ \o -> LayerTreePictureTile
    <$> o A..: "x"
    <*> o A..: "y"
    <*> o A..: "picture"
instance ToJSON LayerTreePictureTile where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (layerTreePictureTileX p),
    ("y" A..=) <$> Just (layerTreePictureTileY p),
    ("picture" A..=) <$> Just (layerTreePictureTilePicture p)
    ]

-- | Type 'LayerTree.Layer'.
--   Information about a compositing layer.
data LayerTreeLayer = LayerTreeLayer
  {
    -- | The unique id for this layer.
    layerTreeLayerLayerId :: LayerTreeLayerId,
    -- | The id of parent (not present for root).
    layerTreeLayerParentLayerId :: Maybe LayerTreeLayerId,
    -- | The backend id for the node associated with this layer.
    layerTreeLayerBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DOMBackendNodeId,
    -- | Offset from parent layer, X coordinate.
    layerTreeLayerOffsetX :: Double,
    -- | Offset from parent layer, Y coordinate.
    layerTreeLayerOffsetY :: Double,
    -- | Layer width.
    layerTreeLayerWidth :: Double,
    -- | Layer height.
    layerTreeLayerHeight :: Double,
    -- | Transformation matrix for layer, default is identity matrix
    layerTreeLayerTransform :: Maybe [Double],
    -- | Transform anchor point X, absent if no transform specified
    layerTreeLayerAnchorX :: Maybe Double,
    -- | Transform anchor point Y, absent if no transform specified
    layerTreeLayerAnchorY :: Maybe Double,
    -- | Transform anchor point Z, absent if no transform specified
    layerTreeLayerAnchorZ :: Maybe Double,
    -- | Indicates how many time this layer has painted.
    layerTreeLayerPaintCount :: Int,
    -- | Indicates whether this layer hosts any content, rather than being used for
    --   transform/scrolling purposes only.
    layerTreeLayerDrawsContent :: Bool,
    -- | Set if layer is not visible.
    layerTreeLayerInvisible :: Maybe Bool,
    -- | Rectangles scrolling on main thread only.
    layerTreeLayerScrollRects :: Maybe [LayerTreeScrollRect],
    -- | Sticky position constraint information
    layerTreeLayerStickyPositionConstraint :: Maybe LayerTreeStickyPositionConstraint
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeLayer where
  parseJSON = A.withObject "LayerTreeLayer" $ \o -> LayerTreeLayer
    <$> o A..: "layerId"
    <*> o A..:? "parentLayerId"
    <*> o A..:? "backendNodeId"
    <*> o A..: "offsetX"
    <*> o A..: "offsetY"
    <*> o A..: "width"
    <*> o A..: "height"
    <*> o A..:? "transform"
    <*> o A..:? "anchorX"
    <*> o A..:? "anchorY"
    <*> o A..:? "anchorZ"
    <*> o A..: "paintCount"
    <*> o A..: "drawsContent"
    <*> o A..:? "invisible"
    <*> o A..:? "scrollRects"
    <*> o A..:? "stickyPositionConstraint"
instance ToJSON LayerTreeLayer where
  toJSON p = A.object $ catMaybes [
    ("layerId" A..=) <$> Just (layerTreeLayerLayerId p),
    ("parentLayerId" A..=) <$> (layerTreeLayerParentLayerId p),
    ("backendNodeId" A..=) <$> (layerTreeLayerBackendNodeId p),
    ("offsetX" A..=) <$> Just (layerTreeLayerOffsetX p),
    ("offsetY" A..=) <$> Just (layerTreeLayerOffsetY p),
    ("width" A..=) <$> Just (layerTreeLayerWidth p),
    ("height" A..=) <$> Just (layerTreeLayerHeight p),
    ("transform" A..=) <$> (layerTreeLayerTransform p),
    ("anchorX" A..=) <$> (layerTreeLayerAnchorX p),
    ("anchorY" A..=) <$> (layerTreeLayerAnchorY p),
    ("anchorZ" A..=) <$> (layerTreeLayerAnchorZ p),
    ("paintCount" A..=) <$> Just (layerTreeLayerPaintCount p),
    ("drawsContent" A..=) <$> Just (layerTreeLayerDrawsContent p),
    ("invisible" A..=) <$> (layerTreeLayerInvisible p),
    ("scrollRects" A..=) <$> (layerTreeLayerScrollRects p),
    ("stickyPositionConstraint" A..=) <$> (layerTreeLayerStickyPositionConstraint p)
    ]

-- | Type 'LayerTree.PaintProfile'.
--   Array of timings, one per paint step.
type LayerTreePaintProfile = [Double]

-- | Type of the 'LayerTree.layerPainted' event.
data LayerTreeLayerPainted = LayerTreeLayerPainted
  {
    -- | The id of the painted layer.
    layerTreeLayerPaintedLayerId :: LayerTreeLayerId,
    -- | Clip rectangle.
    layerTreeLayerPaintedClip :: DOMPageNetworkEmulationSecurity.DOMRect
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeLayerPainted where
  parseJSON = A.withObject "LayerTreeLayerPainted" $ \o -> LayerTreeLayerPainted
    <$> o A..: "layerId"
    <*> o A..: "clip"
instance Event LayerTreeLayerPainted where
  eventName _ = "LayerTree.layerPainted"

-- | Type of the 'LayerTree.layerTreeDidChange' event.
data LayerTreeLayerTreeDidChange = LayerTreeLayerTreeDidChange
  {
    -- | Layer tree, absent if not in the comspositing mode.
    layerTreeLayerTreeDidChangeLayers :: Maybe [LayerTreeLayer]
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeLayerTreeDidChange where
  parseJSON = A.withObject "LayerTreeLayerTreeDidChange" $ \o -> LayerTreeLayerTreeDidChange
    <$> o A..:? "layers"
instance Event LayerTreeLayerTreeDidChange where
  eventName _ = "LayerTree.layerTreeDidChange"

-- | Provides the reasons why the given layer was composited.

-- | Parameters of the 'LayerTree.compositingReasons' command.
data PLayerTreeCompositingReasons = PLayerTreeCompositingReasons
  {
    -- | The id of the layer for which we want to get the reasons it was composited.
    pLayerTreeCompositingReasonsLayerId :: LayerTreeLayerId
  }
  deriving (Eq, Show)
pLayerTreeCompositingReasons
  -- | The id of the layer for which we want to get the reasons it was composited.
  :: LayerTreeLayerId
  -> PLayerTreeCompositingReasons
pLayerTreeCompositingReasons
  arg_pLayerTreeCompositingReasonsLayerId
  = PLayerTreeCompositingReasons
    arg_pLayerTreeCompositingReasonsLayerId
instance ToJSON PLayerTreeCompositingReasons where
  toJSON p = A.object $ catMaybes [
    ("layerId" A..=) <$> Just (pLayerTreeCompositingReasonsLayerId p)
    ]
data LayerTreeCompositingReasons = LayerTreeCompositingReasons
  {
    -- | A list of strings specifying reason IDs for the given layer to become composited.
    layerTreeCompositingReasonsCompositingReasonIds :: [String]
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeCompositingReasons where
  parseJSON = A.withObject "LayerTreeCompositingReasons" $ \o -> LayerTreeCompositingReasons
    <$> o A..: "compositingReasonIds"
instance Command PLayerTreeCompositingReasons where
  type CommandResponse PLayerTreeCompositingReasons = LayerTreeCompositingReasons
  commandName _ = "LayerTree.compositingReasons"

-- | Disables compositing tree inspection.

-- | Parameters of the 'LayerTree.disable' command.
data PLayerTreeDisable = PLayerTreeDisable
  deriving (Eq, Show)
pLayerTreeDisable
  :: PLayerTreeDisable
pLayerTreeDisable
  = PLayerTreeDisable
instance ToJSON PLayerTreeDisable where
  toJSON _ = A.Null
instance Command PLayerTreeDisable where
  type CommandResponse PLayerTreeDisable = ()
  commandName _ = "LayerTree.disable"
  fromJSON = const . A.Success . const ()

-- | Enables compositing tree inspection.

-- | Parameters of the 'LayerTree.enable' command.
data PLayerTreeEnable = PLayerTreeEnable
  deriving (Eq, Show)
pLayerTreeEnable
  :: PLayerTreeEnable
pLayerTreeEnable
  = PLayerTreeEnable
instance ToJSON PLayerTreeEnable where
  toJSON _ = A.Null
instance Command PLayerTreeEnable where
  type CommandResponse PLayerTreeEnable = ()
  commandName _ = "LayerTree.enable"
  fromJSON = const . A.Success . const ()

-- | Returns the snapshot identifier.

-- | Parameters of the 'LayerTree.loadSnapshot' command.
data PLayerTreeLoadSnapshot = PLayerTreeLoadSnapshot
  {
    -- | An array of tiles composing the snapshot.
    pLayerTreeLoadSnapshotTiles :: [LayerTreePictureTile]
  }
  deriving (Eq, Show)
pLayerTreeLoadSnapshot
  -- | An array of tiles composing the snapshot.
  :: [LayerTreePictureTile]
  -> PLayerTreeLoadSnapshot
pLayerTreeLoadSnapshot
  arg_pLayerTreeLoadSnapshotTiles
  = PLayerTreeLoadSnapshot
    arg_pLayerTreeLoadSnapshotTiles
instance ToJSON PLayerTreeLoadSnapshot where
  toJSON p = A.object $ catMaybes [
    ("tiles" A..=) <$> Just (pLayerTreeLoadSnapshotTiles p)
    ]
data LayerTreeLoadSnapshot = LayerTreeLoadSnapshot
  {
    -- | The id of the snapshot.
    layerTreeLoadSnapshotSnapshotId :: LayerTreeSnapshotId
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeLoadSnapshot where
  parseJSON = A.withObject "LayerTreeLoadSnapshot" $ \o -> LayerTreeLoadSnapshot
    <$> o A..: "snapshotId"
instance Command PLayerTreeLoadSnapshot where
  type CommandResponse PLayerTreeLoadSnapshot = LayerTreeLoadSnapshot
  commandName _ = "LayerTree.loadSnapshot"

-- | Returns the layer snapshot identifier.

-- | Parameters of the 'LayerTree.makeSnapshot' command.
data PLayerTreeMakeSnapshot = PLayerTreeMakeSnapshot
  {
    -- | The id of the layer.
    pLayerTreeMakeSnapshotLayerId :: LayerTreeLayerId
  }
  deriving (Eq, Show)
pLayerTreeMakeSnapshot
  -- | The id of the layer.
  :: LayerTreeLayerId
  -> PLayerTreeMakeSnapshot
pLayerTreeMakeSnapshot
  arg_pLayerTreeMakeSnapshotLayerId
  = PLayerTreeMakeSnapshot
    arg_pLayerTreeMakeSnapshotLayerId
instance ToJSON PLayerTreeMakeSnapshot where
  toJSON p = A.object $ catMaybes [
    ("layerId" A..=) <$> Just (pLayerTreeMakeSnapshotLayerId p)
    ]
data LayerTreeMakeSnapshot = LayerTreeMakeSnapshot
  {
    -- | The id of the layer snapshot.
    layerTreeMakeSnapshotSnapshotId :: LayerTreeSnapshotId
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeMakeSnapshot where
  parseJSON = A.withObject "LayerTreeMakeSnapshot" $ \o -> LayerTreeMakeSnapshot
    <$> o A..: "snapshotId"
instance Command PLayerTreeMakeSnapshot where
  type CommandResponse PLayerTreeMakeSnapshot = LayerTreeMakeSnapshot
  commandName _ = "LayerTree.makeSnapshot"


-- | Parameters of the 'LayerTree.profileSnapshot' command.
data PLayerTreeProfileSnapshot = PLayerTreeProfileSnapshot
  {
    -- | The id of the layer snapshot.
    pLayerTreeProfileSnapshotSnapshotId :: LayerTreeSnapshotId,
    -- | The maximum number of times to replay the snapshot (1, if not specified).
    pLayerTreeProfileSnapshotMinRepeatCount :: Maybe Int,
    -- | The minimum duration (in seconds) to replay the snapshot.
    pLayerTreeProfileSnapshotMinDuration :: Maybe Double,
    -- | The clip rectangle to apply when replaying the snapshot.
    pLayerTreeProfileSnapshotClipRect :: Maybe DOMPageNetworkEmulationSecurity.DOMRect
  }
  deriving (Eq, Show)
pLayerTreeProfileSnapshot
  -- | The id of the layer snapshot.
  :: LayerTreeSnapshotId
  -> PLayerTreeProfileSnapshot
pLayerTreeProfileSnapshot
  arg_pLayerTreeProfileSnapshotSnapshotId
  = PLayerTreeProfileSnapshot
    arg_pLayerTreeProfileSnapshotSnapshotId
    Nothing
    Nothing
    Nothing
instance ToJSON PLayerTreeProfileSnapshot where
  toJSON p = A.object $ catMaybes [
    ("snapshotId" A..=) <$> Just (pLayerTreeProfileSnapshotSnapshotId p),
    ("minRepeatCount" A..=) <$> (pLayerTreeProfileSnapshotMinRepeatCount p),
    ("minDuration" A..=) <$> (pLayerTreeProfileSnapshotMinDuration p),
    ("clipRect" A..=) <$> (pLayerTreeProfileSnapshotClipRect p)
    ]
data LayerTreeProfileSnapshot = LayerTreeProfileSnapshot
  {
    -- | The array of paint profiles, one per run.
    layerTreeProfileSnapshotTimings :: [LayerTreePaintProfile]
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeProfileSnapshot where
  parseJSON = A.withObject "LayerTreeProfileSnapshot" $ \o -> LayerTreeProfileSnapshot
    <$> o A..: "timings"
instance Command PLayerTreeProfileSnapshot where
  type CommandResponse PLayerTreeProfileSnapshot = LayerTreeProfileSnapshot
  commandName _ = "LayerTree.profileSnapshot"

-- | Releases layer snapshot captured by the back-end.

-- | Parameters of the 'LayerTree.releaseSnapshot' command.
data PLayerTreeReleaseSnapshot = PLayerTreeReleaseSnapshot
  {
    -- | The id of the layer snapshot.
    pLayerTreeReleaseSnapshotSnapshotId :: LayerTreeSnapshotId
  }
  deriving (Eq, Show)
pLayerTreeReleaseSnapshot
  -- | The id of the layer snapshot.
  :: LayerTreeSnapshotId
  -> PLayerTreeReleaseSnapshot
pLayerTreeReleaseSnapshot
  arg_pLayerTreeReleaseSnapshotSnapshotId
  = PLayerTreeReleaseSnapshot
    arg_pLayerTreeReleaseSnapshotSnapshotId
instance ToJSON PLayerTreeReleaseSnapshot where
  toJSON p = A.object $ catMaybes [
    ("snapshotId" A..=) <$> Just (pLayerTreeReleaseSnapshotSnapshotId p)
    ]
instance Command PLayerTreeReleaseSnapshot where
  type CommandResponse PLayerTreeReleaseSnapshot = ()
  commandName _ = "LayerTree.releaseSnapshot"
  fromJSON = const . A.Success . const ()

-- | Replays the layer snapshot and returns the resulting bitmap.

-- | Parameters of the 'LayerTree.replaySnapshot' command.
data PLayerTreeReplaySnapshot = PLayerTreeReplaySnapshot
  {
    -- | The id of the layer snapshot.
    pLayerTreeReplaySnapshotSnapshotId :: LayerTreeSnapshotId,
    -- | The first step to replay from (replay from the very start if not specified).
    pLayerTreeReplaySnapshotFromStep :: Maybe Int,
    -- | The last step to replay to (replay till the end if not specified).
    pLayerTreeReplaySnapshotToStep :: Maybe Int,
    -- | The scale to apply while replaying (defaults to 1).
    pLayerTreeReplaySnapshotScale :: Maybe Double
  }
  deriving (Eq, Show)
pLayerTreeReplaySnapshot
  -- | The id of the layer snapshot.
  :: LayerTreeSnapshotId
  -> PLayerTreeReplaySnapshot
pLayerTreeReplaySnapshot
  arg_pLayerTreeReplaySnapshotSnapshotId
  = PLayerTreeReplaySnapshot
    arg_pLayerTreeReplaySnapshotSnapshotId
    Nothing
    Nothing
    Nothing
instance ToJSON PLayerTreeReplaySnapshot where
  toJSON p = A.object $ catMaybes [
    ("snapshotId" A..=) <$> Just (pLayerTreeReplaySnapshotSnapshotId p),
    ("fromStep" A..=) <$> (pLayerTreeReplaySnapshotFromStep p),
    ("toStep" A..=) <$> (pLayerTreeReplaySnapshotToStep p),
    ("scale" A..=) <$> (pLayerTreeReplaySnapshotScale p)
    ]
data LayerTreeReplaySnapshot = LayerTreeReplaySnapshot
  {
    -- | A data: URL for resulting image.
    layerTreeReplaySnapshotDataURL :: String
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeReplaySnapshot where
  parseJSON = A.withObject "LayerTreeReplaySnapshot" $ \o -> LayerTreeReplaySnapshot
    <$> o A..: "dataURL"
instance Command PLayerTreeReplaySnapshot where
  type CommandResponse PLayerTreeReplaySnapshot = LayerTreeReplaySnapshot
  commandName _ = "LayerTree.replaySnapshot"

-- | Replays the layer snapshot and returns canvas log.

-- | Parameters of the 'LayerTree.snapshotCommandLog' command.
data PLayerTreeSnapshotCommandLog = PLayerTreeSnapshotCommandLog
  {
    -- | The id of the layer snapshot.
    pLayerTreeSnapshotCommandLogSnapshotId :: LayerTreeSnapshotId
  }
  deriving (Eq, Show)
pLayerTreeSnapshotCommandLog
  -- | The id of the layer snapshot.
  :: LayerTreeSnapshotId
  -> PLayerTreeSnapshotCommandLog
pLayerTreeSnapshotCommandLog
  arg_pLayerTreeSnapshotCommandLogSnapshotId
  = PLayerTreeSnapshotCommandLog
    arg_pLayerTreeSnapshotCommandLogSnapshotId
instance ToJSON PLayerTreeSnapshotCommandLog where
  toJSON p = A.object $ catMaybes [
    ("snapshotId" A..=) <$> Just (pLayerTreeSnapshotCommandLogSnapshotId p)
    ]
data LayerTreeSnapshotCommandLog = LayerTreeSnapshotCommandLog
  {
    -- | The array of canvas function calls.
    layerTreeSnapshotCommandLogCommandLog :: [[(String, String)]]
  }
  deriving (Eq, Show)
instance FromJSON LayerTreeSnapshotCommandLog where
  parseJSON = A.withObject "LayerTreeSnapshotCommandLog" $ \o -> LayerTreeSnapshotCommandLog
    <$> o A..: "commandLog"
instance Command PLayerTreeSnapshotCommandLog where
  type CommandResponse PLayerTreeSnapshotCommandLog = LayerTreeSnapshotCommandLog
  commandName _ = "LayerTree.snapshotCommandLog"

