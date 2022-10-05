{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Input 
-}


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



-- | Type 'Input.TouchPoint' .
data InputTouchPoint = InputTouchPoint {
  -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
  inputTouchPointX :: Double,
  -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    -- the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
  inputTouchPointY :: Double,
  -- | X radius of the touch area (default: 1.0).
  inputTouchPointRadiusX :: Maybe Double,
  -- | Y radius of the touch area (default: 1.0).
  inputTouchPointRadiusY :: Maybe Double,
  -- | Rotation angle (default: 0.0).
  inputTouchPointRotationAngle :: Maybe Double,
  -- | Force (default: 1.0).
  inputTouchPointForce :: Maybe Double,
  -- | The normalized tangential pressure, which has a range of [-1,1] (default: 0).
  inputTouchPointTangentialPressure :: Maybe Double,
  -- | The plane angle between the Y-Z plane and the plane containing both the stylus axis and the Y axis, in degrees of the range [-90,90], a positive tiltX is to the right (default: 0)
  inputTouchPointTiltX :: Maybe Int,
  -- | The plane angle between the X-Z plane and the plane containing both the stylus axis and the X axis, in degrees of the range [-90,90], a positive tiltY is towards the user (default: 0).
  inputTouchPointTiltY :: Maybe Int,
  -- | The clockwise rotation of a pen stylus around its own major axis, in degrees in the range [0,359] (default: 0).
  inputTouchPointTwist :: Maybe Int,
  -- | Identifier used to track touch sources between events, must be unique within an event.
  inputTouchPointId :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputTouchPoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  InputTouchPoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }



-- | Type 'Input.GestureSourceType' .
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



-- | Type 'Input.MouseButton' .
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



-- | UTC time in seconds, counted from January 1, 1970.
type InputTimeSinceEpoch = Double

-- | Type 'Input.DragDataItem' .
data InputDragDataItem = InputDragDataItem {
  -- | Mime type of the dragged data.
  inputDragDataItemMimeType :: String,
  -- | Depending of the value of `mimeType`, it contains the dragged link,
    -- text, HTML markup or any other data.
  inputDragDataItemData :: String,
  -- | Title associated with a link. Only valid when `mimeType` == "text/uri-list".
  inputDragDataItemTitle :: Maybe String,
  -- | Stores the base URL for the contained markup. Only valid when `mimeType`
    -- == "text/html".
  inputDragDataItemBaseUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragDataItem  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  InputDragDataItem where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type 'Input.DragData' .
data InputDragData = InputDragData {
  inputDragDataItems :: [InputDragDataItem],
  -- | List of filenames that should be included when dropping
  inputDragDataFiles :: Maybe [String],
  -- | Bit field representing allowed drag operations. Copy = 1, Link = 2, Move = 16
  inputDragDataDragOperationsMask :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragData  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  InputDragData where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }





-- | Type of the 'Input.dragIntercepted' event.
data InputDragIntercepted = InputDragIntercepted {
  inputDragInterceptedData :: InputDragData
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputDragIntercepted  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  InputDragIntercepted where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Parameters of the 'inputDispatchDragEvent' command.
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
  -- | Type of the drag event.
  pInputDispatchDragEventType :: PInputDispatchDragEventType,
  -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
  pInputDispatchDragEventX :: Double,
  -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    -- the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
  pInputDispatchDragEventY :: Double,
  pInputDispatchDragEventData :: InputDragData,
  -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    -- (default: 0).
  pInputDispatchDragEventModifiers :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchDragEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchDragEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Input.dispatchDragEvent' command.
 -- Dispatches a drag event into the page.
-- Parameters: 'PInputDispatchDragEvent'
inputDispatchDragEvent :: Handle ev -> PInputDispatchDragEvent -> IO ()
inputDispatchDragEvent handle params = sendReceiveCommand handle "Input.dispatchDragEvent" (Just params)


-- | Parameters of the 'inputDispatchKeyEvent' command.
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
  -- | Type of the key event.
  pInputDispatchKeyEventType :: PInputDispatchKeyEventType,
  -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    -- (default: 0).
  pInputDispatchKeyEventModifiers :: Maybe Int,
  -- | Time at which the event occurred.
  pInputDispatchKeyEventTimestamp :: Maybe InputTimeSinceEpoch,
  -- | Text as generated by processing a virtual key code with a keyboard layout. Not needed for
    -- for `keyUp` and `rawKeyDown` events (default: "")
  pInputDispatchKeyEventText :: Maybe String,
  -- | Text that would have been generated by the keyboard if no modifiers were pressed (except for
    -- shift). Useful for shortcut (accelerator) key handling (default: "").
  pInputDispatchKeyEventUnmodifiedText :: Maybe String,
  -- | Unique key identifier (e.g., 'U+0041') (default: "").
  pInputDispatchKeyEventKeyIdentifier :: Maybe String,
  -- | Unique DOM defined string value for each physical key (e.g., 'KeyA') (default: "").
  pInputDispatchKeyEventCode :: Maybe String,
  -- | Unique DOM defined string value describing the meaning of the key in the context of active
    -- modifiers, keyboard layout, etc (e.g., 'AltGr') (default: "").
  pInputDispatchKeyEventKey :: Maybe String,
  -- | Windows virtual key code (default: 0).
  pInputDispatchKeyEventWindowsVirtualKeyCode :: Maybe Int,
  -- | Native virtual key code (default: 0).
  pInputDispatchKeyEventNativeVirtualKeyCode :: Maybe Int,
  -- | Whether the event was generated from auto repeat (default: false).
  pInputDispatchKeyEventAutoRepeat :: Maybe Bool,
  -- | Whether the event was generated from the keypad (default: false).
  pInputDispatchKeyEventIsKeypad :: Maybe Bool,
  -- | Whether the event was a system key event (default: false).
  pInputDispatchKeyEventIsSystemKey :: Maybe Bool,
  -- | Whether the event was from the left or right side of the keyboard. 1=Left, 2=Right (default:
    -- 0).
  pInputDispatchKeyEventLocation :: Maybe Int,
  -- | Editing commands to send with the key event (e.g., 'selectAll') (default: []).
    -- These are related to but not equal the command names used in `document.execCommand` and NSStandardKeyBindingResponding.
    -- See https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/editing/commands/editor_command_names.h for valid command names.
  pInputDispatchKeyEventCommands :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchKeyEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchKeyEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Input.dispatchKeyEvent' command.
 -- Dispatches a key event to the page.
-- Parameters: 'PInputDispatchKeyEvent'
inputDispatchKeyEvent :: Handle ev -> PInputDispatchKeyEvent -> IO ()
inputDispatchKeyEvent handle params = sendReceiveCommand handle "Input.dispatchKeyEvent" (Just params)


-- | Parameters of the 'inputInsertText' command.
data PInputInsertText = PInputInsertText {
  -- | The text to insert.
  pInputInsertTextText :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputInsertText  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PInputInsertText where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'Input.insertText' command.
 -- This method emulates inserting text that doesn't come from a key press,
 -- for example an emoji keyboard or an IME.
-- Parameters: 'PInputInsertText'
inputInsertText :: Handle ev -> PInputInsertText -> IO ()
inputInsertText handle params = sendReceiveCommand handle "Input.insertText" (Just params)


-- | Parameters of the 'inputImeSetComposition' command.
data PInputImeSetComposition = PInputImeSetComposition {
  -- | The text to insert
  pInputImeSetCompositionText :: String,
  -- | selection start
  pInputImeSetCompositionSelectionStart :: Int,
  -- | selection end
  pInputImeSetCompositionSelectionEnd :: Int,
  -- | replacement start
  pInputImeSetCompositionReplacementStart :: Maybe Int,
  -- | replacement end
  pInputImeSetCompositionReplacementEnd :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputImeSetComposition  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputImeSetComposition where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Input.imeSetComposition' command.
 -- This method sets the current candidate text for ime.
 -- Use imeCommitComposition to commit the final text.
 -- Use imeSetComposition with empty string as text to cancel composition.
-- Parameters: 'PInputImeSetComposition'
inputImeSetComposition :: Handle ev -> PInputImeSetComposition -> IO ()
inputImeSetComposition handle params = sendReceiveCommand handle "Input.imeSetComposition" (Just params)


-- | Parameters of the 'inputDispatchMouseEvent' command.
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
  -- | Type of the mouse event.
  pInputDispatchMouseEventType :: PInputDispatchMouseEventType,
  -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
  pInputDispatchMouseEventX :: Double,
  -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    -- the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
  pInputDispatchMouseEventY :: Double,
  -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    -- (default: 0).
  pInputDispatchMouseEventModifiers :: Maybe Int,
  -- | Time at which the event occurred.
  pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
  -- | Mouse button (default: "none").
  pInputDispatchMouseEventButton :: Maybe InputMouseButton,
  -- | A number indicating which buttons are pressed on the mouse when a mouse event is triggered.
    -- Left=1, Right=2, Middle=4, Back=8, Forward=16, None=0.
  pInputDispatchMouseEventButtons :: Maybe Int,
  -- | Number of times the mouse button was clicked (default: 0).
  pInputDispatchMouseEventClickCount :: Maybe Int,
  -- | The normalized pressure, which has a range of [0,1] (default: 0).
  pInputDispatchMouseEventForce :: Maybe Double,
  -- | The normalized tangential pressure, which has a range of [-1,1] (default: 0).
  pInputDispatchMouseEventTangentialPressure :: Maybe Double,
  -- | The plane angle between the Y-Z plane and the plane containing both the stylus axis and the Y axis, in degrees of the range [-90,90], a positive tiltX is to the right (default: 0).
  pInputDispatchMouseEventTiltX :: Maybe Int,
  -- | The plane angle between the X-Z plane and the plane containing both the stylus axis and the X axis, in degrees of the range [-90,90], a positive tiltY is towards the user (default: 0).
  pInputDispatchMouseEventTiltY :: Maybe Int,
  -- | The clockwise rotation of a pen stylus around its own major axis, in degrees in the range [0,359] (default: 0).
  pInputDispatchMouseEventTwist :: Maybe Int,
  -- | X delta in CSS pixels for mouse wheel event (default: 0).
  pInputDispatchMouseEventDeltaX :: Maybe Double,
  -- | Y delta in CSS pixels for mouse wheel event (default: 0).
  pInputDispatchMouseEventDeltaY :: Maybe Double,
  -- | Pointer type (default: "mouse").
  pInputDispatchMouseEventPointerType :: PInputDispatchMouseEventPointerType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchMouseEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchMouseEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Input.dispatchMouseEvent' command.
 -- Dispatches a mouse event to the page.
-- Parameters: 'PInputDispatchMouseEvent'
inputDispatchMouseEvent :: Handle ev -> PInputDispatchMouseEvent -> IO ()
inputDispatchMouseEvent handle params = sendReceiveCommand handle "Input.dispatchMouseEvent" (Just params)


-- | Parameters of the 'inputDispatchTouchEvent' command.
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
  -- | Type of the touch event. TouchEnd and TouchCancel must not contain any touch points, while
    -- TouchStart and TouchMove must contains at least one.
  pInputDispatchTouchEventType :: PInputDispatchTouchEventType,
  -- | Active touch points on the touch device. One event per any changed point (compared to
    -- previous touch event in a sequence) is generated, emulating pressing/moving/releasing points
    -- one by one.
  pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
  -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    -- (default: 0).
  pInputDispatchTouchEventModifiers :: Maybe Int,
  -- | Time at which the event occurred.
  pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchTouchEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchTouchEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Input.dispatchTouchEvent' command.
 -- Dispatches a touch event to the page.
-- Parameters: 'PInputDispatchTouchEvent'
inputDispatchTouchEvent :: Handle ev -> PInputDispatchTouchEvent -> IO ()
inputDispatchTouchEvent handle params = sendReceiveCommand handle "Input.dispatchTouchEvent" (Just params)


-- | Parameters of the 'inputEmulateTouchFromMouseEvent' command.
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
  -- | Type of the mouse event.
  pInputEmulateTouchFromMouseEventType :: PInputEmulateTouchFromMouseEventType,
  -- | X coordinate of the mouse pointer in DIP.
  pInputEmulateTouchFromMouseEventX :: Int,
  -- | Y coordinate of the mouse pointer in DIP.
  pInputEmulateTouchFromMouseEventY :: Int,
  -- | Mouse button. Only "none", "left", "right" are supported.
  pInputEmulateTouchFromMouseEventButton :: InputMouseButton,
  -- | Time at which the event occurred (default: current time).
  pInputEmulateTouchFromMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
  -- | X delta in DIP for mouse wheel event (default: 0).
  pInputEmulateTouchFromMouseEventDeltaX :: Maybe Double,
  -- | Y delta in DIP for mouse wheel event (default: 0).
  pInputEmulateTouchFromMouseEventDeltaY :: Maybe Double,
  -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    -- (default: 0).
  pInputEmulateTouchFromMouseEventModifiers :: Maybe Int,
  -- | Number of times the mouse button was clicked (default: 0).
  pInputEmulateTouchFromMouseEventClickCount :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputEmulateTouchFromMouseEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PInputEmulateTouchFromMouseEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the 'Input.emulateTouchFromMouseEvent' command.
 -- Emulates touch event from the mouse event parameters.
-- Parameters: 'PInputEmulateTouchFromMouseEvent'
inputEmulateTouchFromMouseEvent :: Handle ev -> PInputEmulateTouchFromMouseEvent -> IO ()
inputEmulateTouchFromMouseEvent handle params = sendReceiveCommand handle "Input.emulateTouchFromMouseEvent" (Just params)


-- | Parameters of the 'inputSetIgnoreInputEvents' command.
data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
  -- | Ignores input events processing when set to true.
  pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetIgnoreInputEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSetIgnoreInputEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Input.setIgnoreInputEvents' command.
 -- Ignores input events (useful while auditing page).
-- Parameters: 'PInputSetIgnoreInputEvents'
inputSetIgnoreInputEvents :: Handle ev -> PInputSetIgnoreInputEvents -> IO ()
inputSetIgnoreInputEvents handle params = sendReceiveCommand handle "Input.setIgnoreInputEvents" (Just params)


-- | Parameters of the 'inputSetInterceptDrags' command.
data PInputSetInterceptDrags = PInputSetInterceptDrags {
  pInputSetInterceptDragsEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetInterceptDrags  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PInputSetInterceptDrags where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Input.setInterceptDrags' command.
 -- Prevents default drag and drop behavior and instead emits `Input.dragIntercepted` events.
 -- Drag and drop behavior can be directly controlled via `Input.dispatchDragEvent`.
-- Parameters: 'PInputSetInterceptDrags'
inputSetInterceptDrags :: Handle ev -> PInputSetInterceptDrags -> IO ()
inputSetInterceptDrags handle params = sendReceiveCommand handle "Input.setInterceptDrags" (Just params)


-- | Parameters of the 'inputSynthesizePinchGesture' command.
data PInputSynthesizePinchGesture = PInputSynthesizePinchGesture {
  -- | X coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizePinchGestureX :: Double,
  -- | Y coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizePinchGestureY :: Double,
  -- | Relative scale factor after zooming (>1.0 zooms in, <1.0 zooms out).
  pInputSynthesizePinchGestureScaleFactor :: Double,
  -- | Relative pointer speed in pixels per second (default: 800).
  pInputSynthesizePinchGestureRelativeSpeed :: Maybe Int,
  -- | Which type of input events to be generated (default: 'default', which queries the platform
    -- for the preferred input type).
  pInputSynthesizePinchGestureGestureSourceType :: Maybe InputGestureSourceType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizePinchGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizePinchGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Input.synthesizePinchGesture' command.
 -- Synthesizes a pinch gesture over a time period by issuing appropriate touch events.
-- Parameters: 'PInputSynthesizePinchGesture'
inputSynthesizePinchGesture :: Handle ev -> PInputSynthesizePinchGesture -> IO ()
inputSynthesizePinchGesture handle params = sendReceiveCommand handle "Input.synthesizePinchGesture" (Just params)


-- | Parameters of the 'inputSynthesizeScrollGesture' command.
data PInputSynthesizeScrollGesture = PInputSynthesizeScrollGesture {
  -- | X coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizeScrollGestureX :: Double,
  -- | Y coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizeScrollGestureY :: Double,
  -- | The distance to scroll along the X axis (positive to scroll left).
  pInputSynthesizeScrollGestureXDistance :: Maybe Double,
  -- | The distance to scroll along the Y axis (positive to scroll up).
  pInputSynthesizeScrollGestureYDistance :: Maybe Double,
  -- | The number of additional pixels to scroll back along the X axis, in addition to the given
    -- distance.
  pInputSynthesizeScrollGestureXOverscroll :: Maybe Double,
  -- | The number of additional pixels to scroll back along the Y axis, in addition to the given
    -- distance.
  pInputSynthesizeScrollGestureYOverscroll :: Maybe Double,
  -- | Prevent fling (default: true).
  pInputSynthesizeScrollGesturePreventFling :: Maybe Bool,
  -- | Swipe speed in pixels per second (default: 800).
  pInputSynthesizeScrollGestureSpeed :: Maybe Int,
  -- | Which type of input events to be generated (default: 'default', which queries the platform
    -- for the preferred input type).
  pInputSynthesizeScrollGestureGestureSourceType :: Maybe InputGestureSourceType,
  -- | The number of times to repeat the gesture (default: 0).
  pInputSynthesizeScrollGestureRepeatCount :: Maybe Int,
  -- | The number of milliseconds delay between each repeat. (default: 250).
  pInputSynthesizeScrollGestureRepeatDelayMs :: Maybe Int,
  -- | The name of the interaction markers to generate, if not empty (default: "").
  pInputSynthesizeScrollGestureInteractionMarkerName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizeScrollGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizeScrollGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Input.synthesizeScrollGesture' command.
 -- Synthesizes a scroll gesture over a time period by issuing appropriate touch events.
-- Parameters: 'PInputSynthesizeScrollGesture'
inputSynthesizeScrollGesture :: Handle ev -> PInputSynthesizeScrollGesture -> IO ()
inputSynthesizeScrollGesture handle params = sendReceiveCommand handle "Input.synthesizeScrollGesture" (Just params)


-- | Parameters of the 'inputSynthesizeTapGesture' command.
data PInputSynthesizeTapGesture = PInputSynthesizeTapGesture {
  -- | X coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizeTapGestureX :: Double,
  -- | Y coordinate of the start of the gesture in CSS pixels.
  pInputSynthesizeTapGestureY :: Double,
  -- | Duration between touchdown and touchup events in ms (default: 50).
  pInputSynthesizeTapGestureDuration :: Maybe Int,
  -- | Number of times to perform the tap (e.g. 2 for double tap, default: 1).
  pInputSynthesizeTapGestureTapCount :: Maybe Int,
  -- | Which type of input events to be generated (default: 'default', which queries the platform
    -- for the preferred input type).
  pInputSynthesizeTapGestureGestureSourceType :: Maybe InputGestureSourceType
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSynthesizeTapGesture  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSynthesizeTapGesture where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Input.synthesizeTapGesture' command.
 -- Synthesizes a tap gesture over a time period by issuing appropriate touch events.
-- Parameters: 'PInputSynthesizeTapGesture'
inputSynthesizeTapGesture :: Handle ev -> PInputSynthesizeTapGesture -> IO ()
inputSynthesizeTapGesture handle params = sendReceiveCommand handle "Input.synthesizeTapGesture" (Just params)



