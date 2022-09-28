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


-- | Unique Layer identifier.
type LayerTreeLayerId = String

-- | Unique snapshot identifier.
type LayerTreeSnapshotId = String

-- | Rectangle where scrolling happens on the main thread.
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
   layerTreeScrollRectRect :: LayerTreeScrollRectRect, -- ^ Rectangle itself.
   layerTreeScrollRectType :: LayerTreeScrollRectType -- ^ Reason for rectangle to force scrolling on the main thread
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeScrollRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  LayerTreeScrollRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Sticky position constraints.
data LayerTreeStickyPositionConstraint = LayerTreeStickyPositionConstraint {
   layerTreeStickyPositionConstraintStickyBoxRect :: LayerTreeStickyPositionConstraintStickyBoxRect, -- ^ Layout rectangle of the sticky element before being shifted
   layerTreeStickyPositionConstraintContainingBlockRect :: LayerTreeStickyPositionConstraintContainingBlockRect, -- ^ Layout rectangle of the containing block of the sticky element
   layerTreeStickyPositionConstraintNearestLayerShiftingStickyBox :: LayerTreeStickyPositionConstraintNearestLayerShiftingStickyBox, -- ^ The nearest sticky layer that shifts the sticky box
   layerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock :: LayerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock -- ^ The nearest sticky layer that shifts the containing block
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeStickyPositionConstraint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  LayerTreeStickyPositionConstraint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



-- | Serialized fragment of layer picture along with its offset within the layer.
data LayerTreePictureTile = LayerTreePictureTile {
   layerTreePictureTileX :: LayerTreePictureTileX, -- ^ Offset from owning layer left boundary
   layerTreePictureTileY :: LayerTreePictureTileY, -- ^ Offset from owning layer top boundary
   layerTreePictureTilePicture :: LayerTreePictureTilePicture -- ^ Base64-encoded snapshot data. (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreePictureTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  LayerTreePictureTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



-- | Information about a compositing layer.
data LayerTreeLayer = LayerTreeLayer {
   layerTreeLayerLayerId :: LayerTreeLayerLayerId, -- ^ The unique id for this layer.
   layerTreeLayerParentLayerId :: LayerTreeLayerParentLayerId, -- ^ The id of parent (not present for root).
   layerTreeLayerBackendNodeId :: LayerTreeLayerBackendNodeId, -- ^ The backend id for the node associated with this layer.
   layerTreeLayerOffsetX :: LayerTreeLayerOffsetX, -- ^ Offset from parent layer, X coordinate.
   layerTreeLayerOffsetY :: LayerTreeLayerOffsetY, -- ^ Offset from parent layer, Y coordinate.
   layerTreeLayerWidth :: LayerTreeLayerWidth, -- ^ Layer width.
   layerTreeLayerHeight :: LayerTreeLayerHeight, -- ^ Layer height.
   layerTreeLayerTransform :: LayerTreeLayerTransform, -- ^ Transformation matrix for layer, default is identity matrix
   layerTreeLayerAnchorX :: LayerTreeLayerAnchorX, -- ^ Transform anchor point X, absent if no transform specified
   layerTreeLayerAnchorY :: LayerTreeLayerAnchorY, -- ^ Transform anchor point Y, absent if no transform specified
   layerTreeLayerAnchorZ :: LayerTreeLayerAnchorZ, -- ^ Transform anchor point Z, absent if no transform specified
   layerTreeLayerPaintCount :: LayerTreeLayerPaintCount, -- ^ Indicates how many time this layer has painted.
   layerTreeLayerDrawsContent :: LayerTreeLayerDrawsContent, -- ^ Indicates whether this layer hosts any content, rather than being used for
transform/scrolling purposes only.
   layerTreeLayerInvisible :: LayerTreeLayerInvisible, -- ^ Set if layer is not visible.
   layerTreeLayerScrollRects :: LayerTreeLayerScrollRects, -- ^ Rectangles scrolling on main thread only.
   layerTreeLayerStickyPositionConstraint :: LayerTreeLayerStickyPositionConstraint -- ^ Sticky position constraint information
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }



-- | Array of timings, one per paint step.
type LayerTreePaintProfile = [Double]



-- | Type of the 'LayerTree.layerPainted' event.
data LayerTreeLayerPainted = LayerTreeLayerPainted {
   layerTreeLayerPaintedLayerId :: LayerTreeLayerPaintedLayerId, -- ^ The id of the painted layer.
   layerTreeLayerPaintedClip :: LayerTreeLayerPaintedClip -- ^ Clip rectangle.
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerPainted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerPainted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'LayerTree.layerTreeDidChange' event.
data LayerTreeLayerTreeDidChange = LayerTreeLayerTreeDidChange {
   layerTreeLayerTreeDidChangeLayers :: LayerTreeLayerTreeDidChangeLayers -- ^ Layer tree, absent if not in the comspositing mode.
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerTreeDidChange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerTreeDidChange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





-- | Parameters of the 'layerTreeCompositingReasons' command.
data PLayerTreeCompositingReasons = PLayerTreeCompositingReasons {
   pLayerTreeCompositingReasonsLayerId :: PLayerTreeCompositingReasonsLayerId -- ^ The id of the layer for which we want to get the reasons it was composited.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeCompositingReasons  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'LayerTree.compositingReasons'.
-- Provides the reasons why the given layer was composited.
-- Parameters: 'PLayerTreeCompositingReasons'
-- Returns: 'LayerTreeCompositingReasons'
layerTreeCompositingReasons :: Handle ev -> PLayerTreeCompositingReasons -> IO (Either Error LayerTreeCompositingReasons)
layerTreeCompositingReasons handle params = sendReceiveCommandResult handle "LayerTree.compositingReasons" (Just params)

-- | Return type of the 'layerTreeCompositingReasons' command.
data LayerTreeCompositingReasons = LayerTreeCompositingReasons {
   layerTreeCompositingReasonsCompositingReasonIds :: [String] -- ^ A list of strings specifying reason IDs for the given layer to become composited.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeCompositingReasons where
   commandName _ = "LayerTree.compositingReasons"



-- | Function for the command 'LayerTree.disable'.
-- Disables compositing tree inspection.
layerTreeDisable :: Handle ev -> IO (Maybe Error)
layerTreeDisable handle = sendReceiveCommand handle "LayerTree.disable" (Nothing :: Maybe ())


-- | Function for the command 'LayerTree.enable'.
-- Enables compositing tree inspection.
layerTreeEnable :: Handle ev -> IO (Maybe Error)
layerTreeEnable handle = sendReceiveCommand handle "LayerTree.enable" (Nothing :: Maybe ())


-- | Parameters of the 'layerTreeLoadSnapshot' command.
data PLayerTreeLoadSnapshot = PLayerTreeLoadSnapshot {
   pLayerTreeLoadSnapshotTiles :: PLayerTreeLoadSnapshotTiles -- ^ An array of tiles composing the snapshot.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeLoadSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'LayerTree.loadSnapshot'.
-- Returns the snapshot identifier.
-- Parameters: 'PLayerTreeLoadSnapshot'
-- Returns: 'LayerTreeLoadSnapshot'
layerTreeLoadSnapshot :: Handle ev -> PLayerTreeLoadSnapshot -> IO (Either Error LayerTreeLoadSnapshot)
layerTreeLoadSnapshot handle params = sendReceiveCommandResult handle "LayerTree.loadSnapshot" (Just params)

-- | Return type of the 'layerTreeLoadSnapshot' command.
data LayerTreeLoadSnapshot = LayerTreeLoadSnapshot {
   layerTreeLoadSnapshotSnapshotId :: LayerTreeSnapshotId -- ^ The id of the snapshot.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeLoadSnapshot where
   commandName _ = "LayerTree.loadSnapshot"



-- | Parameters of the 'layerTreeMakeSnapshot' command.
data PLayerTreeMakeSnapshot = PLayerTreeMakeSnapshot {
   pLayerTreeMakeSnapshotLayerId :: PLayerTreeMakeSnapshotLayerId -- ^ The id of the layer.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeMakeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'LayerTree.makeSnapshot'.
-- Returns the layer snapshot identifier.
-- Parameters: 'PLayerTreeMakeSnapshot'
-- Returns: 'LayerTreeMakeSnapshot'
layerTreeMakeSnapshot :: Handle ev -> PLayerTreeMakeSnapshot -> IO (Either Error LayerTreeMakeSnapshot)
layerTreeMakeSnapshot handle params = sendReceiveCommandResult handle "LayerTree.makeSnapshot" (Just params)

-- | Return type of the 'layerTreeMakeSnapshot' command.
data LayerTreeMakeSnapshot = LayerTreeMakeSnapshot {
   layerTreeMakeSnapshotSnapshotId :: LayerTreeSnapshotId -- ^ The id of the layer snapshot.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeMakeSnapshot where
   commandName _ = "LayerTree.makeSnapshot"



-- | Parameters of the 'layerTreeProfileSnapshot' command.
data PLayerTreeProfileSnapshot = PLayerTreeProfileSnapshot {
   pLayerTreeProfileSnapshotSnapshotId :: PLayerTreeProfileSnapshotSnapshotId, -- ^ The id of the layer snapshot.
   pLayerTreeProfileSnapshotMinRepeatCount :: PLayerTreeProfileSnapshotMinRepeatCount, -- ^ The maximum number of times to replay the snapshot (1, if not specified).
   pLayerTreeProfileSnapshotMinDuration :: PLayerTreeProfileSnapshotMinDuration, -- ^ The minimum duration (in seconds) to replay the snapshot.
   pLayerTreeProfileSnapshotClipRect :: PLayerTreeProfileSnapshotClipRect -- ^ The clip rectangle to apply when replaying the snapshot.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeProfileSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'LayerTree.profileSnapshot'.
-- Parameters: 'PLayerTreeProfileSnapshot'
-- Returns: 'LayerTreeProfileSnapshot'
layerTreeProfileSnapshot :: Handle ev -> PLayerTreeProfileSnapshot -> IO (Either Error LayerTreeProfileSnapshot)
layerTreeProfileSnapshot handle params = sendReceiveCommandResult handle "LayerTree.profileSnapshot" (Just params)

-- | Return type of the 'layerTreeProfileSnapshot' command.
data LayerTreeProfileSnapshot = LayerTreeProfileSnapshot {
   layerTreeProfileSnapshotTimings :: [LayerTreePaintProfile] -- ^ The array of paint profiles, one per run.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command LayerTreeProfileSnapshot where
   commandName _ = "LayerTree.profileSnapshot"



-- | Parameters of the 'layerTreeReleaseSnapshot' command.
data PLayerTreeReleaseSnapshot = PLayerTreeReleaseSnapshot {
   pLayerTreeReleaseSnapshotSnapshotId :: PLayerTreeReleaseSnapshotSnapshotId -- ^ The id of the layer snapshot.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReleaseSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReleaseSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'LayerTree.releaseSnapshot'.
-- Releases layer snapshot captured by the back-end.
-- Parameters: 'PLayerTreeReleaseSnapshot'
layerTreeReleaseSnapshot :: Handle ev -> PLayerTreeReleaseSnapshot -> IO (Maybe Error)
layerTreeReleaseSnapshot handle params = sendReceiveCommand handle "LayerTree.releaseSnapshot" (Just params)


-- | Parameters of the 'layerTreeReplaySnapshot' command.
data PLayerTreeReplaySnapshot = PLayerTreeReplaySnapshot {
   pLayerTreeReplaySnapshotSnapshotId :: PLayerTreeReplaySnapshotSnapshotId, -- ^ The id of the layer snapshot.
   pLayerTreeReplaySnapshotFromStep :: PLayerTreeReplaySnapshotFromStep, -- ^ The first step to replay from (replay from the very start if not specified).
   pLayerTreeReplaySnapshotToStep :: PLayerTreeReplaySnapshotToStep, -- ^ The last step to replay to (replay till the end if not specified).
   pLayerTreeReplaySnapshotScale :: PLayerTreeReplaySnapshotScale -- ^ The scale to apply while replaying (defaults to 1).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReplaySnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'LayerTree.replaySnapshot'.
-- Replays the layer snapshot and returns the resulting bitmap.
-- Parameters: 'PLayerTreeReplaySnapshot'
-- Returns: 'LayerTreeReplaySnapshot'
layerTreeReplaySnapshot :: Handle ev -> PLayerTreeReplaySnapshot -> IO (Either Error LayerTreeReplaySnapshot)
layerTreeReplaySnapshot handle params = sendReceiveCommandResult handle "LayerTree.replaySnapshot" (Just params)

-- | Return type of the 'layerTreeReplaySnapshot' command.
data LayerTreeReplaySnapshot = LayerTreeReplaySnapshot {
   layerTreeReplaySnapshotDataUrl :: String -- ^ A data: URL for resulting image.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command LayerTreeReplaySnapshot where
   commandName _ = "LayerTree.replaySnapshot"



-- | Parameters of the 'layerTreeSnapshotCommandLog' command.
data PLayerTreeSnapshotCommandLog = PLayerTreeSnapshotCommandLog {
   pLayerTreeSnapshotCommandLogSnapshotId :: PLayerTreeSnapshotCommandLogSnapshotId -- ^ The id of the layer snapshot.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeSnapshotCommandLog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'LayerTree.snapshotCommandLog'.
-- Replays the layer snapshot and returns canvas log.
-- Parameters: 'PLayerTreeSnapshotCommandLog'
-- Returns: 'LayerTreeSnapshotCommandLog'
layerTreeSnapshotCommandLog :: Handle ev -> PLayerTreeSnapshotCommandLog -> IO (Either Error LayerTreeSnapshotCommandLog)
layerTreeSnapshotCommandLog handle params = sendReceiveCommandResult handle "LayerTree.snapshotCommandLog" (Just params)

-- | Return type of the 'layerTreeSnapshotCommandLog' command.
data LayerTreeSnapshotCommandLog = LayerTreeSnapshotCommandLog {
   layerTreeSnapshotCommandLogCommandLog :: [[(String, String)]] -- ^ The array of canvas function calls.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeSnapshotCommandLog where
   commandName _ = "LayerTree.snapshotCommandLog"




