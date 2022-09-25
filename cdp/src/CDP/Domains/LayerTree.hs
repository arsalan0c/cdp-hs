{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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


type LayerTreeLayerId = String
type LayerTreeSnapshotId = String
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
   layerTreeScrollRectRect :: DOMPageNetworkEmulationSecurity.DomRect,
   layerTreeScrollRectType :: LayerTreeScrollRectType
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeScrollRect  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  LayerTreeScrollRect where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data LayerTreeStickyPositionConstraint = LayerTreeStickyPositionConstraint {
   layerTreeStickyPositionConstraintStickyBoxRect :: DOMPageNetworkEmulationSecurity.DomRect,
   layerTreeStickyPositionConstraintContainingBlockRect :: DOMPageNetworkEmulationSecurity.DomRect,
   layerTreeStickyPositionConstraintNearestLayerShiftingStickyBox :: Maybe LayerTreeLayerId,
   layerTreeStickyPositionConstraintNearestLayerShiftingContainingBlock :: Maybe LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeStickyPositionConstraint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 , A.omitNothingFields = True}

instance FromJSON  LayerTreeStickyPositionConstraint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 33 }



data LayerTreePictureTile = LayerTreePictureTile {
   layerTreePictureTileX :: Double,
   layerTreePictureTileY :: Double,
   layerTreePictureTilePicture :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreePictureTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  LayerTreePictureTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }



data LayerTreeLayer = LayerTreeLayer {
   layerTreeLayerLayerId :: LayerTreeLayerId,
   layerTreeLayerParentLayerId :: Maybe LayerTreeLayerId,
   layerTreeLayerBackendNodeId :: Maybe DOMPageNetworkEmulationSecurity.DomBackendNodeId,
   layerTreeLayerOffsetX :: Double,
   layerTreeLayerOffsetY :: Double,
   layerTreeLayerWidth :: Double,
   layerTreeLayerHeight :: Double,
   layerTreeLayerTransform :: Maybe [Double],
   layerTreeLayerAnchorX :: Maybe Double,
   layerTreeLayerAnchorY :: Maybe Double,
   layerTreeLayerAnchorZ :: Maybe Double,
   layerTreeLayerPaintCount :: Int,
   layerTreeLayerDrawsContent :: Bool,
   layerTreeLayerInvisible :: Maybe Bool,
   layerTreeLayerScrollRects :: Maybe [LayerTreeScrollRect],
   layerTreeLayerStickyPositionConstraint :: Maybe LayerTreeStickyPositionConstraint
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayer  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayer where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 14 }


type LayerTreePaintProfile = [Double]



data LayerTreeLayerPainted = LayerTreeLayerPainted {
   layerTreeLayerPaintedLayerId :: LayerTreeLayerId,
   layerTreeLayerPaintedClip :: DOMPageNetworkEmulationSecurity.DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerPainted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerPainted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data LayerTreeLayerTreeDidChange = LayerTreeLayerTreeDidChange {
   layerTreeLayerTreeDidChangeLayers :: Maybe [LayerTreeLayer]
} deriving (Generic, Eq, Show, Read)
instance ToJSON LayerTreeLayerTreeDidChange  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  LayerTreeLayerTreeDidChange where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }





data PLayerTreeCompositingReasons = PLayerTreeCompositingReasons {
   pLayerTreeCompositingReasonsLayerId :: LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeCompositingReasons  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


layerTreeCompositingReasons :: Handle ev -> PLayerTreeCompositingReasons -> IO (Either Error LayerTreeCompositingReasons)
layerTreeCompositingReasons handle params = sendReceiveCommandResult handle "LayerTree.compositingReasons" (Just params)

data LayerTreeCompositingReasons = LayerTreeCompositingReasons {
   layerTreeCompositingReasonsCompositingReasonIds :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeCompositingReasons where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeCompositingReasons where
   commandName _ = "LayerTree.compositingReasons"



layerTreeDisable :: Handle ev -> IO (Maybe Error)
layerTreeDisable handle = sendReceiveCommand handle "LayerTree.disable" (Nothing :: Maybe ())


layerTreeEnable :: Handle ev -> IO (Maybe Error)
layerTreeEnable handle = sendReceiveCommand handle "LayerTree.enable" (Nothing :: Maybe ())



data PLayerTreeLoadSnapshot = PLayerTreeLoadSnapshot {
   pLayerTreeLoadSnapshotTiles :: [LayerTreePictureTile]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeLoadSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


layerTreeLoadSnapshot :: Handle ev -> PLayerTreeLoadSnapshot -> IO (Either Error LayerTreeLoadSnapshot)
layerTreeLoadSnapshot handle params = sendReceiveCommandResult handle "LayerTree.loadSnapshot" (Just params)

data LayerTreeLoadSnapshot = LayerTreeLoadSnapshot {
   layerTreeLoadSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeLoadSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeLoadSnapshot where
   commandName _ = "LayerTree.loadSnapshot"




data PLayerTreeMakeSnapshot = PLayerTreeMakeSnapshot {
   pLayerTreeMakeSnapshotLayerId :: LayerTreeLayerId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeMakeSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


layerTreeMakeSnapshot :: Handle ev -> PLayerTreeMakeSnapshot -> IO (Either Error LayerTreeMakeSnapshot)
layerTreeMakeSnapshot handle params = sendReceiveCommandResult handle "LayerTree.makeSnapshot" (Just params)

data LayerTreeMakeSnapshot = LayerTreeMakeSnapshot {
   layerTreeMakeSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeMakeSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command LayerTreeMakeSnapshot where
   commandName _ = "LayerTree.makeSnapshot"




data PLayerTreeProfileSnapshot = PLayerTreeProfileSnapshot {
   pLayerTreeProfileSnapshotSnapshotId :: LayerTreeSnapshotId,
   pLayerTreeProfileSnapshotMinRepeatCount :: Maybe Int,
   pLayerTreeProfileSnapshotMinDuration :: Maybe Double,
   pLayerTreeProfileSnapshotClipRect :: Maybe DOMPageNetworkEmulationSecurity.DomRect
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeProfileSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


layerTreeProfileSnapshot :: Handle ev -> PLayerTreeProfileSnapshot -> IO (Either Error LayerTreeProfileSnapshot)
layerTreeProfileSnapshot handle params = sendReceiveCommandResult handle "LayerTree.profileSnapshot" (Just params)

data LayerTreeProfileSnapshot = LayerTreeProfileSnapshot {
   layerTreeProfileSnapshotTimings :: [LayerTreePaintProfile]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeProfileSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command LayerTreeProfileSnapshot where
   commandName _ = "LayerTree.profileSnapshot"




data PLayerTreeReleaseSnapshot = PLayerTreeReleaseSnapshot {
   pLayerTreeReleaseSnapshotSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReleaseSnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReleaseSnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


layerTreeReleaseSnapshot :: Handle ev -> PLayerTreeReleaseSnapshot -> IO (Maybe Error)
layerTreeReleaseSnapshot handle params = sendReceiveCommand handle "LayerTree.releaseSnapshot" (Just params)



data PLayerTreeReplaySnapshot = PLayerTreeReplaySnapshot {
   pLayerTreeReplaySnapshotSnapshotId :: LayerTreeSnapshotId,
   pLayerTreeReplaySnapshotFromStep :: Maybe Int,
   pLayerTreeReplaySnapshotToStep :: Maybe Int,
   pLayerTreeReplaySnapshotScale :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeReplaySnapshot  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


layerTreeReplaySnapshot :: Handle ev -> PLayerTreeReplaySnapshot -> IO (Either Error LayerTreeReplaySnapshot)
layerTreeReplaySnapshot handle params = sendReceiveCommandResult handle "LayerTree.replaySnapshot" (Just params)

data LayerTreeReplaySnapshot = LayerTreeReplaySnapshot {
   layerTreeReplaySnapshotDataUrl :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeReplaySnapshot where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }

instance Command LayerTreeReplaySnapshot where
   commandName _ = "LayerTree.replaySnapshot"




data PLayerTreeSnapshotCommandLog = PLayerTreeSnapshotCommandLog {
   pLayerTreeSnapshotCommandLogSnapshotId :: LayerTreeSnapshotId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PLayerTreeSnapshotCommandLog  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PLayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


layerTreeSnapshotCommandLog :: Handle ev -> PLayerTreeSnapshotCommandLog -> IO (Either Error LayerTreeSnapshotCommandLog)
layerTreeSnapshotCommandLog handle params = sendReceiveCommandResult handle "LayerTree.snapshotCommandLog" (Just params)

data LayerTreeSnapshotCommandLog = LayerTreeSnapshotCommandLog {
   layerTreeSnapshotCommandLogCommandLog :: [[(String, String)]]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  LayerTreeSnapshotCommandLog where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command LayerTreeSnapshotCommandLog where
   commandName _ = "LayerTree.snapshotCommandLog"




