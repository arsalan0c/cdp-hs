{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  LayerTree 
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
   parseJSON = A.withText  "LayerTreeScrollRectType"  $ \v -> do
      case v of
         "RepaintsOnScroll" -> pure LayerTreeScrollRectTypeRepaintsOnScroll
         "TouchEventHandler" -> pure LayerTreeScrollRectTypeTouchEventHandler
         "WheelEventHandler" -> pure LayerTreeScrollRectTypeWheelEventHandler
         _ -> fail "failed to parse LayerTreeScrollRectType"

instance ToJSON LayerTreeScrollRectType where
   toJSON v = A.String $
      case v of
         LayerTreeScrollRectTypeRepaintsOnScroll -> "RepaintsOnScroll"
         LayerTreeScrollRectTypeTouchEventHandler -> "TouchEventHandler"
         LayerTreeScrollRectTypeWheelEventHandler -> "WheelEventHandler"



data LayerTreeScrollRect = LayerTreeScrollRect {
  -- | Rectangle itself.
  layerTreeScrollRectRect :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | Reason for rectangle to force scrolling on the main thread
  layerTreeScrollRectType :: LayerTreeScrollRectType
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeScrollRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  LayerTreeScrollRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type 'LayerTree.StickyPositionConstraint'.
--   Sticky position constraints.
data LayerTreeStickyPositionConstraint = LayerTreeStickyPositionConstraint {
  -- | Layout rectangle of the sticky element before being shifted
  layerTreeStickyPositionConstraintStickyBoxRect :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | Layout rectangle of the containing block of the sticky element
  layerTreeStickyPositionConstraintContainingBlockRect :: DOMPageNetworkEmulationSecurity.DomRect,
  -- | The nearest sticky layer that shifts the sticky box
  layerTreeStickyPositionConstraintNearestLayerShiftingStickyBox :: Maybe LayerTreeLayerId,
  -- | The nearest sticky layer that shifts the containing block
  layerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock :: Maybe LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeStickyPositionConstraint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  LayerTreeStickyPositionConstraint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Type 'LayerTree.PictureTile'.
--   Serialized fragment of layer picture along with its offset within the layer.
data LayerTreePictureTile = LayerTreePictureTile {
  -- | Offset from owning layer left boundary
  layerTreePictureTileX :: Double,
  -- | Offset from owning layer top boundary
  layerTreePictureTileY :: Double,
  -- | Base64-encoded snapshot data. (Encoded as a base64 string when passed over JSON)
  layerTreePictureTilePicture :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreePictureTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  LayerTreePictureTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Type 'LayerTree.Layer'.
--   Information about a compositing layer.
data LayerTreeLayer = LayerTreeLayer {
  -- | The unique id for this layer.
  layerTreeLayerLayerId :: LayerTreeLayerId,
  -- | The id of parent (not present for root).
  layerTreeLayerParentLayerId :: Maybe LayerTreeLayerId,
  -- | The backend id for the node associated with this layer.
  layerTreeLayerBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Type 'LayerTree.PaintProfile'.
--   Array of timings, one per paint step.
type LayerTreePaintProfile = [Double]



-- | Type of the 'LayerTree.layerPainted' event.
data LayerTreeLayerPainted = LayerTreeLayerPainted {
  -- | The id of the painted layer.
  layerTreeLayerPaintedLayerId :: LayerTreeLayerId,
  -- | Clip rectangle.
  layerTreeLayerPaintedClip :: DOMPageNetworkEmulationSecurity.DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerPainted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerPainted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'LayerTree.layerTreeDidChange' event.
data LayerTreeLayerTreeDidChange = LayerTreeLayerTreeDidChange {
  -- | Layer tree, absent if not in the comspositing mode.
  layerTreeLayerTreeDidChangeLayers :: Maybe [LayerTreeLayer]
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerTreeDidChange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerTreeDidChange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Parameters of the 'layerTreeCompositingReasons' command.
data PLayerTreeCompositingReasons = PLayerTreeCompositingReasons {
  -- | The id of the layer for which we want to get the reasons it was composited.
  pLayerTreeCompositingReasonsLayerId :: LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeCompositingReasons  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'LayerTree.compositingReasons' command.
--   Provides the reasons why the given layer was composited.
--   Parameters: 'PLayerTreeCompositingReasons'
--   Returns: 'LayerTreeCompositingReasons'
layerTreeCompositingReasons :: Handle ev -> PLayerTreeCompositingReasons -> IO LayerTreeCompositingReasons
layerTreeCompositingReasons handle params = sendReceiveCommandResult handle "LayerTree.compositingReasons" (Just params)

-- | Return type of the 'layerTreeCompositingReasons' command.
data LayerTreeCompositingReasons = LayerTreeCompositingReasons {
  -- | A list of strings specifying reason IDs for the given layer to become composited.
  layerTreeCompositingReasonsCompositingReasonIds :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeCompositingReasons where
   commandName _ = "LayerTree.compositingReasons"



-- | Function for the 'LayerTree.disable' command.
--   Disables compositing tree inspection.
layerTreeDisable :: Handle ev -> IO ()
layerTreeDisable handle = sendReceiveCommand handle "LayerTree.disable" (Nothing :: Maybe ())


-- | Function for the 'LayerTree.enable' command.
--   Enables compositing tree inspection.
layerTreeEnable :: Handle ev -> IO ()
layerTreeEnable handle = sendReceiveCommand handle "LayerTree.enable" (Nothing :: Maybe ())


-- | Parameters of the 'layerTreeLoadSnapshot' command.
data PLayerTreeLoadSnapshot = PLayerTreeLoadSnapshot {
  -- | An array of tiles composing the snapshot.
  pLayerTreeLoadSnapshotTiles :: [LayerTreePictureTile]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeLoadSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'LayerTree.loadSnapshot' command.
--   Returns the snapshot identifier.
--   Parameters: 'PLayerTreeLoadSnapshot'
--   Returns: 'LayerTreeLoadSnapshot'
layerTreeLoadSnapshot :: Handle ev -> PLayerTreeLoadSnapshot -> IO LayerTreeLoadSnapshot
layerTreeLoadSnapshot handle params = sendReceiveCommandResult handle "LayerTree.loadSnapshot" (Just params)

-- | Return type of the 'layerTreeLoadSnapshot' command.
data LayerTreeLoadSnapshot = LayerTreeLoadSnapshot {
  -- | The id of the snapshot.
  layerTreeLoadSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeLoadSnapshot where
   commandName _ = "LayerTree.loadSnapshot"



-- | Parameters of the 'layerTreeMakeSnapshot' command.
data PLayerTreeMakeSnapshot = PLayerTreeMakeSnapshot {
  -- | The id of the layer.
  pLayerTreeMakeSnapshotLayerId :: LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeMakeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'LayerTree.makeSnapshot' command.
--   Returns the layer snapshot identifier.
--   Parameters: 'PLayerTreeMakeSnapshot'
--   Returns: 'LayerTreeMakeSnapshot'
layerTreeMakeSnapshot :: Handle ev -> PLayerTreeMakeSnapshot -> IO LayerTreeMakeSnapshot
layerTreeMakeSnapshot handle params = sendReceiveCommandResult handle "LayerTree.makeSnapshot" (Just params)

-- | Return type of the 'layerTreeMakeSnapshot' command.
data LayerTreeMakeSnapshot = LayerTreeMakeSnapshot {
  -- | The id of the layer snapshot.
  layerTreeMakeSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeMakeSnapshot where
   commandName _ = "LayerTree.makeSnapshot"



-- | Parameters of the 'layerTreeProfileSnapshot' command.
data PLayerTreeProfileSnapshot = PLayerTreeProfileSnapshot {
  -- | The id of the layer snapshot.
  pLayerTreeProfileSnapshotSnapshotId :: LayerTreeSnapshotId,
  -- | The maximum number of times to replay the snapshot (1, if not specified).
  pLayerTreeProfileSnapshotMinRepeatCount :: Maybe Int,
  -- | The minimum duration (in seconds) to replay the snapshot.
  pLayerTreeProfileSnapshotMinDuration :: Maybe Double,
  -- | The clip rectangle to apply when replaying the snapshot.
  pLayerTreeProfileSnapshotClipRect :: Maybe DOMPageNetworkEmulationSecurity.DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeProfileSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'LayerTree.profileSnapshot' command.
--   
--   Parameters: 'PLayerTreeProfileSnapshot'
--   Returns: 'LayerTreeProfileSnapshot'
layerTreeProfileSnapshot :: Handle ev -> PLayerTreeProfileSnapshot -> IO LayerTreeProfileSnapshot
layerTreeProfileSnapshot handle params = sendReceiveCommandResult handle "LayerTree.profileSnapshot" (Just params)

-- | Return type of the 'layerTreeProfileSnapshot' command.
data LayerTreeProfileSnapshot = LayerTreeProfileSnapshot {
  -- | The array of paint profiles, one per run.
  layerTreeProfileSnapshotTimings :: [LayerTreePaintProfile]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command LayerTreeProfileSnapshot where
   commandName _ = "LayerTree.profileSnapshot"



-- | Parameters of the 'layerTreeReleaseSnapshot' command.
data PLayerTreeReleaseSnapshot = PLayerTreeReleaseSnapshot {
  -- | The id of the layer snapshot.
  pLayerTreeReleaseSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReleaseSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReleaseSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'LayerTree.releaseSnapshot' command.
--   Releases layer snapshot captured by the back-end.
--   Parameters: 'PLayerTreeReleaseSnapshot'
layerTreeReleaseSnapshot :: Handle ev -> PLayerTreeReleaseSnapshot -> IO ()
layerTreeReleaseSnapshot handle params = sendReceiveCommand handle "LayerTree.releaseSnapshot" (Just params)


-- | Parameters of the 'layerTreeReplaySnapshot' command.
data PLayerTreeReplaySnapshot = PLayerTreeReplaySnapshot {
  -- | The id of the layer snapshot.
  pLayerTreeReplaySnapshotSnapshotId :: LayerTreeSnapshotId,
  -- | The first step to replay from (replay from the very start if not specified).
  pLayerTreeReplaySnapshotFromStep :: Maybe Int,
  -- | The last step to replay to (replay till the end if not specified).
  pLayerTreeReplaySnapshotToStep :: Maybe Int,
  -- | The scale to apply while replaying (defaults to 1).
  pLayerTreeReplaySnapshotScale :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReplaySnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'LayerTree.replaySnapshot' command.
--   Replays the layer snapshot and returns the resulting bitmap.
--   Parameters: 'PLayerTreeReplaySnapshot'
--   Returns: 'LayerTreeReplaySnapshot'
layerTreeReplaySnapshot :: Handle ev -> PLayerTreeReplaySnapshot -> IO LayerTreeReplaySnapshot
layerTreeReplaySnapshot handle params = sendReceiveCommandResult handle "LayerTree.replaySnapshot" (Just params)

-- | Return type of the 'layerTreeReplaySnapshot' command.
data LayerTreeReplaySnapshot = LayerTreeReplaySnapshot {
  -- | A data: URL for resulting image.
  layerTreeReplaySnapshotDataUrl :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command LayerTreeReplaySnapshot where
   commandName _ = "LayerTree.replaySnapshot"



-- | Parameters of the 'layerTreeSnapshotCommandLog' command.
data PLayerTreeSnapshotCommandLog = PLayerTreeSnapshotCommandLog {
  -- | The id of the layer snapshot.
  pLayerTreeSnapshotCommandLogSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeSnapshotCommandLog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'LayerTree.snapshotCommandLog' command.
--   Replays the layer snapshot and returns canvas log.
--   Parameters: 'PLayerTreeSnapshotCommandLog'
--   Returns: 'LayerTreeSnapshotCommandLog'
layerTreeSnapshotCommandLog :: Handle ev -> PLayerTreeSnapshotCommandLog -> IO LayerTreeSnapshotCommandLog
layerTreeSnapshotCommandLog handle params = sendReceiveCommandResult handle "LayerTree.snapshotCommandLog" (Just params)

-- | Return type of the 'layerTreeSnapshotCommandLog' command.
data LayerTreeSnapshotCommandLog = LayerTreeSnapshotCommandLog {
  -- | The array of canvas function calls.
  layerTreeSnapshotCommandLogCommandLog :: [[(String, String)]]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeSnapshotCommandLog where
   commandName _ = "LayerTree.snapshotCommandLog"




