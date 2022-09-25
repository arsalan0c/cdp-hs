{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Input (module CDP.Domains.Input) where

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




data InputTouchPoint = InputTouchPoint {
   inputTouchPointX :: Double,
   inputTouchPointY :: Double,
   inputTouchPointRadiusX :: Maybe Double,
   inputTouchPointRadiusY :: Maybe Double,
   inputTouchPointRotationAngle :: Maybe Double,
   inputTouchPointForce :: Maybe Double,
   inputTouchPointTangentialPressure :: Maybe Double,
   inputTouchPointTiltX :: Maybe Int,
   inputTouchPointTiltY :: Maybe Int,
   inputTouchPointTwist :: Maybe Int,
   inputTouchPointId :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputTouchPoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  InputTouchPoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


data InputGestureSourceType = InputGestureSourceTypeDefault | InputGestureSourceTypeTouch | InputGestureSourceTypeMouse
   deriving (Ord, Eq, Show, Read)
instance FromJSON InputGestureSourceType where
   parseJSON = A.withText  "InputGestureSourceType"  $ \v -> do
      case v of
         "default" -> pure InputGestureSourceTypeDefault
         "touch" -> pure InputGestureSourceTypeTouch
         "mouse" -> pure InputGestureSourceTypeMouse
         _ -> fail "failed to parse InputGestureSourceType"

instance ToJSON InputGestureSourceType where
   toJSON v = A.String $
      case v of
         InputGestureSourceTypeDefault -> "default"
         InputGestureSourceTypeTouch -> "touch"
         InputGestureSourceTypeMouse -> "mouse"


data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
   deriving (Ord, Eq, Show, Read)
instance FromJSON InputMouseButton where
   parseJSON = A.withText  "InputMouseButton"  $ \v -> do
      case v of
         "none" -> pure InputMouseButtonNone
         "left" -> pure InputMouseButtonLeft
         "middle" -> pure InputMouseButtonMiddle
         "right" -> pure InputMouseButtonRight
         "back" -> pure InputMouseButtonBack
         "forward" -> pure InputMouseButtonForward
         _ -> fail "failed to parse InputMouseButton"

instance ToJSON InputMouseButton where
   toJSON v = A.String $
      case v of
         InputMouseButtonNone -> "none"
         InputMouseButtonLeft -> "left"
         InputMouseButtonMiddle -> "middle"
         InputMouseButtonRight -> "right"
         InputMouseButtonBack -> "back"
         InputMouseButtonForward -> "forward"


type InputTimeSinceEpoch = Double

data InputDragDataItem = InputDragDataItem {
   inputDragDataItemMimeType :: String,
   inputDragDataItemData :: String,
   inputDragDataItemTitle :: Maybe String,
   inputDragDataItemBaseUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragDataItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  InputDragDataItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



data InputDragData = InputDragData {
   inputDragDataItems :: [InputDragDataItem],
   inputDragDataFiles :: Maybe [String],
   inputDragDataDragOperationsMask :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  InputDragData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }





data InputDragIntercepted = InputDragIntercepted {
   inputDragInterceptedData :: InputDragData
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragIntercepted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  InputDragIntercepted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }




data PInputDispatchDragEventType = PInputDispatchDragEventTypeDragEnter | PInputDispatchDragEventTypeDragOver | PInputDispatchDragEventTypeDrop | PInputDispatchDragEventTypeDragCancel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchDragEventType where
   parseJSON = A.withText  "PInputDispatchDragEventType"  $ \v -> do
      case v of
         "dragEnter" -> pure PInputDispatchDragEventTypeDragEnter
         "dragOver" -> pure PInputDispatchDragEventTypeDragOver
         "drop" -> pure PInputDispatchDragEventTypeDrop
         "dragCancel" -> pure PInputDispatchDragEventTypeDragCancel
         _ -> fail "failed to parse PInputDispatchDragEventType"

instance ToJSON PInputDispatchDragEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchDragEventTypeDragEnter -> "dragEnter"
         PInputDispatchDragEventTypeDragOver -> "dragOver"
         PInputDispatchDragEventTypeDrop -> "drop"
         PInputDispatchDragEventTypeDragCancel -> "dragCancel"



data PInputDispatchDragEvent = PInputDispatchDragEvent {
   pInputDispatchDragEventType :: PInputDispatchDragEventType,
   pInputDispatchDragEventX :: Double,
   pInputDispatchDragEventY :: Double,
   pInputDispatchDragEventData :: InputDragData,
   pInputDispatchDragEventModifiers :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchDragEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchDragEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


inputDispatchDragEvent :: Handle ev -> PInputDispatchDragEvent -> IO (Maybe Error)
inputDispatchDragEvent handle params = sendReceiveCommand handle "Input.dispatchDragEvent" (Just params)


data PInputDispatchKeyEventType = PInputDispatchKeyEventTypeKeyDown | PInputDispatchKeyEventTypeKeyUp | PInputDispatchKeyEventTypeRawKeyDown | PInputDispatchKeyEventTypeChar
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchKeyEventType where
   parseJSON = A.withText  "PInputDispatchKeyEventType"  $ \v -> do
      case v of
         "keyDown" -> pure PInputDispatchKeyEventTypeKeyDown
         "keyUp" -> pure PInputDispatchKeyEventTypeKeyUp
         "rawKeyDown" -> pure PInputDispatchKeyEventTypeRawKeyDown
         "char" -> pure PInputDispatchKeyEventTypeChar
         _ -> fail "failed to parse PInputDispatchKeyEventType"

instance ToJSON PInputDispatchKeyEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchKeyEventTypeKeyDown -> "keyDown"
         PInputDispatchKeyEventTypeKeyUp -> "keyUp"
         PInputDispatchKeyEventTypeRawKeyDown -> "rawKeyDown"
         PInputDispatchKeyEventTypeChar -> "char"



data PInputDispatchKeyEvent = PInputDispatchKeyEvent {
   pInputDispatchKeyEventType :: PInputDispatchKeyEventType,
   pInputDispatchKeyEventModifiers :: Maybe Int,
   pInputDispatchKeyEventTimestamp :: Maybe InputTimeSinceEpoch,
   pInputDispatchKeyEventText :: Maybe String,
   pInputDispatchKeyEventUnmodifiedText :: Maybe String,
   pInputDispatchKeyEventKeyIdentifier :: Maybe String,
   pInputDispatchKeyEventCode :: Maybe String,
   pInputDispatchKeyEventKey :: Maybe String,
   pInputDispatchKeyEventWindowsVirtualKeyCode :: Maybe Int,
   pInputDispatchKeyEventNativeVirtualKeyCode :: Maybe Int,
   pInputDispatchKeyEventAutoRepeat :: Maybe Bool,
   pInputDispatchKeyEventIsKeypad :: Maybe Bool,
   pInputDispatchKeyEventIsSystemKey :: Maybe Bool,
   pInputDispatchKeyEventLocation :: Maybe Int,
   pInputDispatchKeyEventCommands :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchKeyEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchKeyEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


inputDispatchKeyEvent :: Handle ev -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent handle params = sendReceiveCommand handle "Input.dispatchKeyEvent" (Just params)



data PInputInsertText = PInputInsertText {
   pInputInsertTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputInsertText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PInputInsertText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


inputInsertText :: Handle ev -> PInputInsertText -> IO (Maybe Error)
inputInsertText handle params = sendReceiveCommand handle "Input.insertText" (Just params)



data PInputImeSetComposition = PInputImeSetComposition {
   pInputImeSetCompositionText :: String,
   pInputImeSetCompositionSelectionStart :: Int,
   pInputImeSetCompositionSelectionEnd :: Int,
   pInputImeSetCompositionReplacementStart :: Maybe Int,
   pInputImeSetCompositionReplacementEnd :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputImeSetComposition  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputImeSetComposition where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


inputImeSetComposition :: Handle ev -> PInputImeSetComposition -> IO (Maybe Error)
inputImeSetComposition handle params = sendReceiveCommand handle "Input.imeSetComposition" (Just params)


data PInputDispatchMouseEventType = PInputDispatchMouseEventTypeMousePressed | PInputDispatchMouseEventTypeMouseReleased | PInputDispatchMouseEventTypeMouseMoved | PInputDispatchMouseEventTypeMouseWheel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventType where
   parseJSON = A.withText  "PInputDispatchMouseEventType"  $ \v -> do
      case v of
         "mousePressed" -> pure PInputDispatchMouseEventTypeMousePressed
         "mouseReleased" -> pure PInputDispatchMouseEventTypeMouseReleased
         "mouseMoved" -> pure PInputDispatchMouseEventTypeMouseMoved
         "mouseWheel" -> pure PInputDispatchMouseEventTypeMouseWheel
         _ -> fail "failed to parse PInputDispatchMouseEventType"

instance ToJSON PInputDispatchMouseEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchMouseEventTypeMousePressed -> "mousePressed"
         PInputDispatchMouseEventTypeMouseReleased -> "mouseReleased"
         PInputDispatchMouseEventTypeMouseMoved -> "mouseMoved"
         PInputDispatchMouseEventTypeMouseWheel -> "mouseWheel"


data PInputDispatchMouseEventPointerType = PInputDispatchMouseEventPointerTypeMouse | PInputDispatchMouseEventPointerTypePen
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventPointerType where
   parseJSON = A.withText  "PInputDispatchMouseEventPointerType"  $ \v -> do
      case v of
         "mouse" -> pure PInputDispatchMouseEventPointerTypeMouse
         "pen" -> pure PInputDispatchMouseEventPointerTypePen
         _ -> fail "failed to parse PInputDispatchMouseEventPointerType"

instance ToJSON PInputDispatchMouseEventPointerType where
   toJSON v = A.String $
      case v of
         PInputDispatchMouseEventPointerTypeMouse -> "mouse"
         PInputDispatchMouseEventPointerTypePen -> "pen"



data PInputDispatchMouseEvent = PInputDispatchMouseEvent {
   pInputDispatchMouseEventType :: PInputDispatchMouseEventType,
   pInputDispatchMouseEventX :: Double,
   pInputDispatchMouseEventY :: Double,
   pInputDispatchMouseEventModifiers :: Maybe Int,
   pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
   pInputDispatchMouseEventButton :: Maybe InputMouseButton,
   pInputDispatchMouseEventButtons :: Maybe Int,
   pInputDispatchMouseEventClickCount :: Maybe Int,
   pInputDispatchMouseEventForce :: Maybe Double,
   pInputDispatchMouseEventTangentialPressure :: Maybe Double,
   pInputDispatchMouseEventTiltX :: Maybe Int,
   pInputDispatchMouseEventTiltY :: Maybe Int,
   pInputDispatchMouseEventTwist :: Maybe Int,
   pInputDispatchMouseEventDeltaX :: Maybe Double,
   pInputDispatchMouseEventDeltaY :: Maybe Double,
   pInputDispatchMouseEventPointerType :: PInputDispatchMouseEventPointerType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchMouseEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchMouseEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


inputDispatchMouseEvent :: Handle ev -> PInputDispatchMouseEvent -> IO (Maybe Error)
inputDispatchMouseEvent handle params = sendReceiveCommand handle "Input.dispatchMouseEvent" (Just params)


data PInputDispatchTouchEventType = PInputDispatchTouchEventTypeTouchStart | PInputDispatchTouchEventTypeTouchEnd | PInputDispatchTouchEventTypeTouchMove | PInputDispatchTouchEventTypeTouchCancel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchTouchEventType where
   parseJSON = A.withText  "PInputDispatchTouchEventType"  $ \v -> do
      case v of
         "touchStart" -> pure PInputDispatchTouchEventTypeTouchStart
         "touchEnd" -> pure PInputDispatchTouchEventTypeTouchEnd
         "touchMove" -> pure PInputDispatchTouchEventTypeTouchMove
         "touchCancel" -> pure PInputDispatchTouchEventTypeTouchCancel
         _ -> fail "failed to parse PInputDispatchTouchEventType"

instance ToJSON PInputDispatchTouchEventType where
   toJSON v = A.String $
      case v of
         PInputDispatchTouchEventTypeTouchStart -> "touchStart"
         PInputDispatchTouchEventTypeTouchEnd -> "touchEnd"
         PInputDispatchTouchEventTypeTouchMove -> "touchMove"
         PInputDispatchTouchEventTypeTouchCancel -> "touchCancel"



data PInputDispatchTouchEvent = PInputDispatchTouchEvent {
   pInputDispatchTouchEventType :: PInputDispatchTouchEventType,
   pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
   pInputDispatchTouchEventModifiers :: Maybe Int,
   pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchTouchEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchTouchEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


inputDispatchTouchEvent :: Handle ev -> PInputDispatchTouchEvent -> IO (Maybe Error)
inputDispatchTouchEvent handle params = sendReceiveCommand handle "Input.dispatchTouchEvent" (Just params)


data PInputEmulateTouchFromMouseEventType = PInputEmulateTouchFromMouseEventTypeMousePressed | PInputEmulateTouchFromMouseEventTypeMouseReleased | PInputEmulateTouchFromMouseEventTypeMouseMoved | PInputEmulateTouchFromMouseEventTypeMouseWheel
   deriving (Ord, Eq, Show, Read)
instance FromJSON PInputEmulateTouchFromMouseEventType where
   parseJSON = A.withText  "PInputEmulateTouchFromMouseEventType"  $ \v -> do
      case v of
         "mousePressed" -> pure PInputEmulateTouchFromMouseEventTypeMousePressed
         "mouseReleased" -> pure PInputEmulateTouchFromMouseEventTypeMouseReleased
         "mouseMoved" -> pure PInputEmulateTouchFromMouseEventTypeMouseMoved
         "mouseWheel" -> pure PInputEmulateTouchFromMouseEventTypeMouseWheel
         _ -> fail "failed to parse PInputEmulateTouchFromMouseEventType"

instance ToJSON PInputEmulateTouchFromMouseEventType where
   toJSON v = A.String $
      case v of
         PInputEmulateTouchFromMouseEventTypeMousePressed -> "mousePressed"
         PInputEmulateTouchFromMouseEventTypeMouseReleased -> "mouseReleased"
         PInputEmulateTouchFromMouseEventTypeMouseMoved -> "mouseMoved"
         PInputEmulateTouchFromMouseEventTypeMouseWheel -> "mouseWheel"



data PInputEmulateTouchFromMouseEvent = PInputEmulateTouchFromMouseEvent {
   pInputEmulateTouchFromMouseEventType :: PInputEmulateTouchFromMouseEventType,
   pInputEmulateTouchFromMouseEventX :: Int,
   pInputEmulateTouchFromMouseEventY :: Int,
   pInputEmulateTouchFromMouseEventButton :: InputMouseButton,
   pInputEmulateTouchFromMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
   pInputEmulateTouchFromMouseEventDeltaX :: Maybe Double,
   pInputEmulateTouchFromMouseEventDeltaY :: Maybe Double,
   pInputEmulateTouchFromMouseEventModifiers :: Maybe Int,
   pInputEmulateTouchFromMouseEventClickCount :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputEmulateTouchFromMouseEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PInputEmulateTouchFromMouseEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


inputEmulateTouchFromMouseEvent :: Handle ev -> PInputEmulateTouchFromMouseEvent -> IO (Maybe Error)
inputEmulateTouchFromMouseEvent handle params = sendReceiveCommand handle "Input.emulateTouchFromMouseEvent" (Just params)



data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
   pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetIgnoreInputEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSetIgnoreInputEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


inputSetIgnoreInputEvents :: Handle ev -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents handle params = sendReceiveCommand handle "Input.setIgnoreInputEvents" (Just params)



data PInputSetInterceptDrags = PInputSetInterceptDrags {
   pInputSetInterceptDragsEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetInterceptDrags  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputSetInterceptDrags where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


inputSetInterceptDrags :: Handle ev -> PInputSetInterceptDrags -> IO (Maybe Error)
inputSetInterceptDrags handle params = sendReceiveCommand handle "Input.setInterceptDrags" (Just params)



data PInputSynthesizePinchGesture = PInputSynthesizePinchGesture {
   pInputSynthesizePinchGestureX :: Double,
   pInputSynthesizePinchGestureY :: Double,
   pInputSynthesizePinchGestureScaleFactor :: Double,
   pInputSynthesizePinchGestureRelativeSpeed :: Maybe Int,
   pInputSynthesizePinchGestureGestureSourceType :: Maybe InputGestureSourceType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizePinchGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizePinchGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


inputSynthesizePinchGesture :: Handle ev -> PInputSynthesizePinchGesture -> IO (Maybe Error)
inputSynthesizePinchGesture handle params = sendReceiveCommand handle "Input.synthesizePinchGesture" (Just params)



data PInputSynthesizeScrollGesture = PInputSynthesizeScrollGesture {
   pInputSynthesizeScrollGestureX :: Double,
   pInputSynthesizeScrollGestureY :: Double,
   pInputSynthesizeScrollGestureXDistance :: Maybe Double,
   pInputSynthesizeScrollGestureYDistance :: Maybe Double,
   pInputSynthesizeScrollGestureXOverscroll :: Maybe Double,
   pInputSynthesizeScrollGestureYOverscroll :: Maybe Double,
   pInputSynthesizeScrollGesturePreventFling :: Maybe Bool,
   pInputSynthesizeScrollGestureSpeed :: Maybe Int,
   pInputSynthesizeScrollGestureGestureSourceType :: Maybe InputGestureSourceType,
   pInputSynthesizeScrollGestureRepeatCount :: Maybe Int,
   pInputSynthesizeScrollGestureRepeatDelayMs :: Maybe Int,
   pInputSynthesizeScrollGestureInteractionMarkerName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizeScrollGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizeScrollGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


inputSynthesizeScrollGesture :: Handle ev -> PInputSynthesizeScrollGesture -> IO (Maybe Error)
inputSynthesizeScrollGesture handle params = sendReceiveCommand handle "Input.synthesizeScrollGesture" (Just params)



data PInputSynthesizeTapGesture = PInputSynthesizeTapGesture {
   pInputSynthesizeTapGestureX :: Double,
   pInputSynthesizeTapGestureY :: Double,
   pInputSynthesizeTapGestureDuration :: Maybe Int,
   pInputSynthesizeTapGestureTapCount :: Maybe Int,
   pInputSynthesizeTapGestureGestureSourceType :: Maybe InputGestureSourceType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizeTapGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizeTapGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


inputSynthesizeTapGesture :: Handle ev -> PInputSynthesizeTapGesture -> IO (Maybe Error)
inputSynthesizeTapGesture handle params = sendReceiveCommand handle "Input.synthesizeTapGesture" (Just params)



