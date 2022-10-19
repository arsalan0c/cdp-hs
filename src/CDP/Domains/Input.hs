{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Input

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'Input.TouchPoint'.
data InputTouchPoint = InputTouchPoint
  {
    -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
    inputTouchPointX :: Double,
    -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    --   the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
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
  }
  deriving (Eq, Show)
instance FromJSON InputTouchPoint where
  parseJSON = A.withObject "InputTouchPoint" $ \o -> InputTouchPoint
    <$> o A..: "x"
    <*> o A..: "y"
    <*> o A..:? "radiusX"
    <*> o A..:? "radiusY"
    <*> o A..:? "rotationAngle"
    <*> o A..:? "force"
    <*> o A..:? "tangentialPressure"
    <*> o A..:? "tiltX"
    <*> o A..:? "tiltY"
    <*> o A..:? "twist"
    <*> o A..:? "id"
instance ToJSON InputTouchPoint where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (inputTouchPointX p),
    ("y" A..=) <$> Just (inputTouchPointY p),
    ("radiusX" A..=) <$> (inputTouchPointRadiusX p),
    ("radiusY" A..=) <$> (inputTouchPointRadiusY p),
    ("rotationAngle" A..=) <$> (inputTouchPointRotationAngle p),
    ("force" A..=) <$> (inputTouchPointForce p),
    ("tangentialPressure" A..=) <$> (inputTouchPointTangentialPressure p),
    ("tiltX" A..=) <$> (inputTouchPointTiltX p),
    ("tiltY" A..=) <$> (inputTouchPointTiltY p),
    ("twist" A..=) <$> (inputTouchPointTwist p),
    ("id" A..=) <$> (inputTouchPointId p)
    ]

-- | Type 'Input.GestureSourceType'.
data InputGestureSourceType = InputGestureSourceTypeDefault | InputGestureSourceTypeTouch | InputGestureSourceTypeMouse
  deriving (Ord, Eq, Show, Read)
instance FromJSON InputGestureSourceType where
  parseJSON = A.withText "InputGestureSourceType" $ \v -> case v of
    "default" -> pure InputGestureSourceTypeDefault
    "touch" -> pure InputGestureSourceTypeTouch
    "mouse" -> pure InputGestureSourceTypeMouse
    "_" -> fail "failed to parse InputGestureSourceType"
instance ToJSON InputGestureSourceType where
  toJSON v = A.String $ case v of
    InputGestureSourceTypeDefault -> "default"
    InputGestureSourceTypeTouch -> "touch"
    InputGestureSourceTypeMouse -> "mouse"

-- | Type 'Input.MouseButton'.
data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
  deriving (Ord, Eq, Show, Read)
instance FromJSON InputMouseButton where
  parseJSON = A.withText "InputMouseButton" $ \v -> case v of
    "none" -> pure InputMouseButtonNone
    "left" -> pure InputMouseButtonLeft
    "middle" -> pure InputMouseButtonMiddle
    "right" -> pure InputMouseButtonRight
    "back" -> pure InputMouseButtonBack
    "forward" -> pure InputMouseButtonForward
    "_" -> fail "failed to parse InputMouseButton"
instance ToJSON InputMouseButton where
  toJSON v = A.String $ case v of
    InputMouseButtonNone -> "none"
    InputMouseButtonLeft -> "left"
    InputMouseButtonMiddle -> "middle"
    InputMouseButtonRight -> "right"
    InputMouseButtonBack -> "back"
    InputMouseButtonForward -> "forward"

-- | Type 'Input.TimeSinceEpoch'.
--   UTC time in seconds, counted from January 1, 1970.
type InputTimeSinceEpoch = Double

-- | Type 'Input.DragDataItem'.
data InputDragDataItem = InputDragDataItem
  {
    -- | Mime type of the dragged data.
    inputDragDataItemMimeType :: T.Text,
    -- | Depending of the value of `mimeType`, it contains the dragged link,
    --   text, HTML markup or any other data.
    inputDragDataItemData :: T.Text,
    -- | Title associated with a link. Only valid when `mimeType` == "text/uri-list".
    inputDragDataItemTitle :: Maybe T.Text,
    -- | Stores the base URL for the contained markup. Only valid when `mimeType`
    --   == "text/html".
    inputDragDataItemBaseURL :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON InputDragDataItem where
  parseJSON = A.withObject "InputDragDataItem" $ \o -> InputDragDataItem
    <$> o A..: "mimeType"
    <*> o A..: "data"
    <*> o A..:? "title"
    <*> o A..:? "baseURL"
instance ToJSON InputDragDataItem where
  toJSON p = A.object $ catMaybes [
    ("mimeType" A..=) <$> Just (inputDragDataItemMimeType p),
    ("data" A..=) <$> Just (inputDragDataItemData p),
    ("title" A..=) <$> (inputDragDataItemTitle p),
    ("baseURL" A..=) <$> (inputDragDataItemBaseURL p)
    ]

-- | Type 'Input.DragData'.
data InputDragData = InputDragData
  {
    inputDragDataItems :: [InputDragDataItem],
    -- | List of filenames that should be included when dropping
    inputDragDataFiles :: Maybe [T.Text],
    -- | Bit field representing allowed drag operations. Copy = 1, Link = 2, Move = 16
    inputDragDataDragOperationsMask :: Int
  }
  deriving (Eq, Show)
instance FromJSON InputDragData where
  parseJSON = A.withObject "InputDragData" $ \o -> InputDragData
    <$> o A..: "items"
    <*> o A..:? "files"
    <*> o A..: "dragOperationsMask"
instance ToJSON InputDragData where
  toJSON p = A.object $ catMaybes [
    ("items" A..=) <$> Just (inputDragDataItems p),
    ("files" A..=) <$> (inputDragDataFiles p),
    ("dragOperationsMask" A..=) <$> Just (inputDragDataDragOperationsMask p)
    ]

-- | Type of the 'Input.dragIntercepted' event.
data InputDragIntercepted = InputDragIntercepted
  {
    inputDragInterceptedData :: InputDragData
  }
  deriving (Eq, Show)
instance FromJSON InputDragIntercepted where
  parseJSON = A.withObject "InputDragIntercepted" $ \o -> InputDragIntercepted
    <$> o A..: "data"
instance Event InputDragIntercepted where
  eventName _ = "Input.dragIntercepted"

-- | Dispatches a drag event into the page.

-- | Parameters of the 'Input.dispatchDragEvent' command.
data PInputDispatchDragEventType = PInputDispatchDragEventTypeDragEnter | PInputDispatchDragEventTypeDragOver | PInputDispatchDragEventTypeDrop | PInputDispatchDragEventTypeDragCancel
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchDragEventType where
  parseJSON = A.withText "PInputDispatchDragEventType" $ \v -> case v of
    "dragEnter" -> pure PInputDispatchDragEventTypeDragEnter
    "dragOver" -> pure PInputDispatchDragEventTypeDragOver
    "drop" -> pure PInputDispatchDragEventTypeDrop
    "dragCancel" -> pure PInputDispatchDragEventTypeDragCancel
    "_" -> fail "failed to parse PInputDispatchDragEventType"
instance ToJSON PInputDispatchDragEventType where
  toJSON v = A.String $ case v of
    PInputDispatchDragEventTypeDragEnter -> "dragEnter"
    PInputDispatchDragEventTypeDragOver -> "dragOver"
    PInputDispatchDragEventTypeDrop -> "drop"
    PInputDispatchDragEventTypeDragCancel -> "dragCancel"
data PInputDispatchDragEvent = PInputDispatchDragEvent
  {
    -- | Type of the drag event.
    pInputDispatchDragEventType :: PInputDispatchDragEventType,
    -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
    pInputDispatchDragEventX :: Double,
    -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    --   the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
    pInputDispatchDragEventY :: Double,
    pInputDispatchDragEventData :: InputDragData,
    -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    --   (default: 0).
    pInputDispatchDragEventModifiers :: Maybe Int
  }
  deriving (Eq, Show)
pInputDispatchDragEvent
  -- | Type of the drag event.
  :: PInputDispatchDragEventType
  -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
  -> Double
  -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
  --   the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
  -> Double
  -> InputDragData
  -> PInputDispatchDragEvent
pInputDispatchDragEvent
  arg_pInputDispatchDragEventType
  arg_pInputDispatchDragEventX
  arg_pInputDispatchDragEventY
  arg_pInputDispatchDragEventData
  = PInputDispatchDragEvent
    arg_pInputDispatchDragEventType
    arg_pInputDispatchDragEventX
    arg_pInputDispatchDragEventY
    arg_pInputDispatchDragEventData
    Nothing
instance ToJSON PInputDispatchDragEvent where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pInputDispatchDragEventType p),
    ("x" A..=) <$> Just (pInputDispatchDragEventX p),
    ("y" A..=) <$> Just (pInputDispatchDragEventY p),
    ("data" A..=) <$> Just (pInputDispatchDragEventData p),
    ("modifiers" A..=) <$> (pInputDispatchDragEventModifiers p)
    ]
instance Command PInputDispatchDragEvent where
  type CommandResponse PInputDispatchDragEvent = ()
  commandName _ = "Input.dispatchDragEvent"
  fromJSON = const . A.Success . const ()

-- | Dispatches a key event to the page.

-- | Parameters of the 'Input.dispatchKeyEvent' command.
data PInputDispatchKeyEventType = PInputDispatchKeyEventTypeKeyDown | PInputDispatchKeyEventTypeKeyUp | PInputDispatchKeyEventTypeRawKeyDown | PInputDispatchKeyEventTypeChar
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchKeyEventType where
  parseJSON = A.withText "PInputDispatchKeyEventType" $ \v -> case v of
    "keyDown" -> pure PInputDispatchKeyEventTypeKeyDown
    "keyUp" -> pure PInputDispatchKeyEventTypeKeyUp
    "rawKeyDown" -> pure PInputDispatchKeyEventTypeRawKeyDown
    "char" -> pure PInputDispatchKeyEventTypeChar
    "_" -> fail "failed to parse PInputDispatchKeyEventType"
instance ToJSON PInputDispatchKeyEventType where
  toJSON v = A.String $ case v of
    PInputDispatchKeyEventTypeKeyDown -> "keyDown"
    PInputDispatchKeyEventTypeKeyUp -> "keyUp"
    PInputDispatchKeyEventTypeRawKeyDown -> "rawKeyDown"
    PInputDispatchKeyEventTypeChar -> "char"
data PInputDispatchKeyEvent = PInputDispatchKeyEvent
  {
    -- | Type of the key event.
    pInputDispatchKeyEventType :: PInputDispatchKeyEventType,
    -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    --   (default: 0).
    pInputDispatchKeyEventModifiers :: Maybe Int,
    -- | Time at which the event occurred.
    pInputDispatchKeyEventTimestamp :: Maybe InputTimeSinceEpoch,
    -- | Text as generated by processing a virtual key code with a keyboard layout. Not needed for
    --   for `keyUp` and `rawKeyDown` events (default: "")
    pInputDispatchKeyEventText :: Maybe T.Text,
    -- | Text that would have been generated by the keyboard if no modifiers were pressed (except for
    --   shift). Useful for shortcut (accelerator) key handling (default: "").
    pInputDispatchKeyEventUnmodifiedText :: Maybe T.Text,
    -- | Unique key identifier (e.g., 'U+0041') (default: "").
    pInputDispatchKeyEventKeyIdentifier :: Maybe T.Text,
    -- | Unique DOM defined string value for each physical key (e.g., 'KeyA') (default: "").
    pInputDispatchKeyEventCode :: Maybe T.Text,
    -- | Unique DOM defined string value describing the meaning of the key in the context of active
    --   modifiers, keyboard layout, etc (e.g., 'AltGr') (default: "").
    pInputDispatchKeyEventKey :: Maybe T.Text,
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
    --   0).
    pInputDispatchKeyEventLocation :: Maybe Int,
    -- | Editing commands to send with the key event (e.g., 'selectAll') (default: []).
    --   These are related to but not equal the command names used in `document.execCommand` and NSStandardKeyBindingResponding.
    --   See https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/editing/commands/editor_command_names.h for valid command names.
    pInputDispatchKeyEventCommands :: Maybe [T.Text]
  }
  deriving (Eq, Show)
pInputDispatchKeyEvent
  -- | Type of the key event.
  :: PInputDispatchKeyEventType
  -> PInputDispatchKeyEvent
pInputDispatchKeyEvent
  arg_pInputDispatchKeyEventType
  = PInputDispatchKeyEvent
    arg_pInputDispatchKeyEventType
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PInputDispatchKeyEvent where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pInputDispatchKeyEventType p),
    ("modifiers" A..=) <$> (pInputDispatchKeyEventModifiers p),
    ("timestamp" A..=) <$> (pInputDispatchKeyEventTimestamp p),
    ("text" A..=) <$> (pInputDispatchKeyEventText p),
    ("unmodifiedText" A..=) <$> (pInputDispatchKeyEventUnmodifiedText p),
    ("keyIdentifier" A..=) <$> (pInputDispatchKeyEventKeyIdentifier p),
    ("code" A..=) <$> (pInputDispatchKeyEventCode p),
    ("key" A..=) <$> (pInputDispatchKeyEventKey p),
    ("windowsVirtualKeyCode" A..=) <$> (pInputDispatchKeyEventWindowsVirtualKeyCode p),
    ("nativeVirtualKeyCode" A..=) <$> (pInputDispatchKeyEventNativeVirtualKeyCode p),
    ("autoRepeat" A..=) <$> (pInputDispatchKeyEventAutoRepeat p),
    ("isKeypad" A..=) <$> (pInputDispatchKeyEventIsKeypad p),
    ("isSystemKey" A..=) <$> (pInputDispatchKeyEventIsSystemKey p),
    ("location" A..=) <$> (pInputDispatchKeyEventLocation p),
    ("commands" A..=) <$> (pInputDispatchKeyEventCommands p)
    ]
instance Command PInputDispatchKeyEvent where
  type CommandResponse PInputDispatchKeyEvent = ()
  commandName _ = "Input.dispatchKeyEvent"
  fromJSON = const . A.Success . const ()

-- | This method emulates inserting text that doesn't come from a key press,
--   for example an emoji keyboard or an IME.

-- | Parameters of the 'Input.insertText' command.
data PInputInsertText = PInputInsertText
  {
    -- | The text to insert.
    pInputInsertTextText :: T.Text
  }
  deriving (Eq, Show)
pInputInsertText
  -- | The text to insert.
  :: T.Text
  -> PInputInsertText
pInputInsertText
  arg_pInputInsertTextText
  = PInputInsertText
    arg_pInputInsertTextText
instance ToJSON PInputInsertText where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (pInputInsertTextText p)
    ]
instance Command PInputInsertText where
  type CommandResponse PInputInsertText = ()
  commandName _ = "Input.insertText"
  fromJSON = const . A.Success . const ()

-- | This method sets the current candidate text for ime.
--   Use imeCommitComposition to commit the final text.
--   Use imeSetComposition with empty string as text to cancel composition.

-- | Parameters of the 'Input.imeSetComposition' command.
data PInputImeSetComposition = PInputImeSetComposition
  {
    -- | The text to insert
    pInputImeSetCompositionText :: T.Text,
    -- | selection start
    pInputImeSetCompositionSelectionStart :: Int,
    -- | selection end
    pInputImeSetCompositionSelectionEnd :: Int,
    -- | replacement start
    pInputImeSetCompositionReplacementStart :: Maybe Int,
    -- | replacement end
    pInputImeSetCompositionReplacementEnd :: Maybe Int
  }
  deriving (Eq, Show)
pInputImeSetComposition
  -- | The text to insert
  :: T.Text
  -- | selection start
  -> Int
  -- | selection end
  -> Int
  -> PInputImeSetComposition
pInputImeSetComposition
  arg_pInputImeSetCompositionText
  arg_pInputImeSetCompositionSelectionStart
  arg_pInputImeSetCompositionSelectionEnd
  = PInputImeSetComposition
    arg_pInputImeSetCompositionText
    arg_pInputImeSetCompositionSelectionStart
    arg_pInputImeSetCompositionSelectionEnd
    Nothing
    Nothing
instance ToJSON PInputImeSetComposition where
  toJSON p = A.object $ catMaybes [
    ("text" A..=) <$> Just (pInputImeSetCompositionText p),
    ("selectionStart" A..=) <$> Just (pInputImeSetCompositionSelectionStart p),
    ("selectionEnd" A..=) <$> Just (pInputImeSetCompositionSelectionEnd p),
    ("replacementStart" A..=) <$> (pInputImeSetCompositionReplacementStart p),
    ("replacementEnd" A..=) <$> (pInputImeSetCompositionReplacementEnd p)
    ]
instance Command PInputImeSetComposition where
  type CommandResponse PInputImeSetComposition = ()
  commandName _ = "Input.imeSetComposition"
  fromJSON = const . A.Success . const ()

-- | Dispatches a mouse event to the page.

-- | Parameters of the 'Input.dispatchMouseEvent' command.
data PInputDispatchMouseEventType = PInputDispatchMouseEventTypeMousePressed | PInputDispatchMouseEventTypeMouseReleased | PInputDispatchMouseEventTypeMouseMoved | PInputDispatchMouseEventTypeMouseWheel
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventType where
  parseJSON = A.withText "PInputDispatchMouseEventType" $ \v -> case v of
    "mousePressed" -> pure PInputDispatchMouseEventTypeMousePressed
    "mouseReleased" -> pure PInputDispatchMouseEventTypeMouseReleased
    "mouseMoved" -> pure PInputDispatchMouseEventTypeMouseMoved
    "mouseWheel" -> pure PInputDispatchMouseEventTypeMouseWheel
    "_" -> fail "failed to parse PInputDispatchMouseEventType"
instance ToJSON PInputDispatchMouseEventType where
  toJSON v = A.String $ case v of
    PInputDispatchMouseEventTypeMousePressed -> "mousePressed"
    PInputDispatchMouseEventTypeMouseReleased -> "mouseReleased"
    PInputDispatchMouseEventTypeMouseMoved -> "mouseMoved"
    PInputDispatchMouseEventTypeMouseWheel -> "mouseWheel"
data PInputDispatchMouseEventPointerType = PInputDispatchMouseEventPointerTypeMouse | PInputDispatchMouseEventPointerTypePen
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchMouseEventPointerType where
  parseJSON = A.withText "PInputDispatchMouseEventPointerType" $ \v -> case v of
    "mouse" -> pure PInputDispatchMouseEventPointerTypeMouse
    "pen" -> pure PInputDispatchMouseEventPointerTypePen
    "_" -> fail "failed to parse PInputDispatchMouseEventPointerType"
instance ToJSON PInputDispatchMouseEventPointerType where
  toJSON v = A.String $ case v of
    PInputDispatchMouseEventPointerTypeMouse -> "mouse"
    PInputDispatchMouseEventPointerTypePen -> "pen"
data PInputDispatchMouseEvent = PInputDispatchMouseEvent
  {
    -- | Type of the mouse event.
    pInputDispatchMouseEventType :: PInputDispatchMouseEventType,
    -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
    pInputDispatchMouseEventX :: Double,
    -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
    --   the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
    pInputDispatchMouseEventY :: Double,
    -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    --   (default: 0).
    pInputDispatchMouseEventModifiers :: Maybe Int,
    -- | Time at which the event occurred.
    pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
    -- | Mouse button (default: "none").
    pInputDispatchMouseEventButton :: Maybe InputMouseButton,
    -- | A number indicating which buttons are pressed on the mouse when a mouse event is triggered.
    --   Left=1, Right=2, Middle=4, Back=8, Forward=16, None=0.
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
    pInputDispatchMouseEventPointerType :: Maybe PInputDispatchMouseEventPointerType
  }
  deriving (Eq, Show)
pInputDispatchMouseEvent
  -- | Type of the mouse event.
  :: PInputDispatchMouseEventType
  -- | X coordinate of the event relative to the main frame's viewport in CSS pixels.
  -> Double
  -- | Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to
  --   the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
  -> Double
  -> PInputDispatchMouseEvent
pInputDispatchMouseEvent
  arg_pInputDispatchMouseEventType
  arg_pInputDispatchMouseEventX
  arg_pInputDispatchMouseEventY
  = PInputDispatchMouseEvent
    arg_pInputDispatchMouseEventType
    arg_pInputDispatchMouseEventX
    arg_pInputDispatchMouseEventY
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PInputDispatchMouseEvent where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pInputDispatchMouseEventType p),
    ("x" A..=) <$> Just (pInputDispatchMouseEventX p),
    ("y" A..=) <$> Just (pInputDispatchMouseEventY p),
    ("modifiers" A..=) <$> (pInputDispatchMouseEventModifiers p),
    ("timestamp" A..=) <$> (pInputDispatchMouseEventTimestamp p),
    ("button" A..=) <$> (pInputDispatchMouseEventButton p),
    ("buttons" A..=) <$> (pInputDispatchMouseEventButtons p),
    ("clickCount" A..=) <$> (pInputDispatchMouseEventClickCount p),
    ("force" A..=) <$> (pInputDispatchMouseEventForce p),
    ("tangentialPressure" A..=) <$> (pInputDispatchMouseEventTangentialPressure p),
    ("tiltX" A..=) <$> (pInputDispatchMouseEventTiltX p),
    ("tiltY" A..=) <$> (pInputDispatchMouseEventTiltY p),
    ("twist" A..=) <$> (pInputDispatchMouseEventTwist p),
    ("deltaX" A..=) <$> (pInputDispatchMouseEventDeltaX p),
    ("deltaY" A..=) <$> (pInputDispatchMouseEventDeltaY p),
    ("pointerType" A..=) <$> (pInputDispatchMouseEventPointerType p)
    ]
instance Command PInputDispatchMouseEvent where
  type CommandResponse PInputDispatchMouseEvent = ()
  commandName _ = "Input.dispatchMouseEvent"
  fromJSON = const . A.Success . const ()

-- | Dispatches a touch event to the page.

-- | Parameters of the 'Input.dispatchTouchEvent' command.
data PInputDispatchTouchEventType = PInputDispatchTouchEventTypeTouchStart | PInputDispatchTouchEventTypeTouchEnd | PInputDispatchTouchEventTypeTouchMove | PInputDispatchTouchEventTypeTouchCancel
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputDispatchTouchEventType where
  parseJSON = A.withText "PInputDispatchTouchEventType" $ \v -> case v of
    "touchStart" -> pure PInputDispatchTouchEventTypeTouchStart
    "touchEnd" -> pure PInputDispatchTouchEventTypeTouchEnd
    "touchMove" -> pure PInputDispatchTouchEventTypeTouchMove
    "touchCancel" -> pure PInputDispatchTouchEventTypeTouchCancel
    "_" -> fail "failed to parse PInputDispatchTouchEventType"
instance ToJSON PInputDispatchTouchEventType where
  toJSON v = A.String $ case v of
    PInputDispatchTouchEventTypeTouchStart -> "touchStart"
    PInputDispatchTouchEventTypeTouchEnd -> "touchEnd"
    PInputDispatchTouchEventTypeTouchMove -> "touchMove"
    PInputDispatchTouchEventTypeTouchCancel -> "touchCancel"
data PInputDispatchTouchEvent = PInputDispatchTouchEvent
  {
    -- | Type of the touch event. TouchEnd and TouchCancel must not contain any touch points, while
    --   TouchStart and TouchMove must contains at least one.
    pInputDispatchTouchEventType :: PInputDispatchTouchEventType,
    -- | Active touch points on the touch device. One event per any changed point (compared to
    --   previous touch event in a sequence) is generated, emulating pressing/moving/releasing points
    --   one by one.
    pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
    -- | Bit field representing pressed modifier keys. Alt=1, Ctrl=2, Meta/Command=4, Shift=8
    --   (default: 0).
    pInputDispatchTouchEventModifiers :: Maybe Int,
    -- | Time at which the event occurred.
    pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
  }
  deriving (Eq, Show)
pInputDispatchTouchEvent
  -- | Type of the touch event. TouchEnd and TouchCancel must not contain any touch points, while
  --   TouchStart and TouchMove must contains at least one.
  :: PInputDispatchTouchEventType
  -- | Active touch points on the touch device. One event per any changed point (compared to
  --   previous touch event in a sequence) is generated, emulating pressing/moving/releasing points
  --   one by one.
  -> [InputTouchPoint]
  -> PInputDispatchTouchEvent
pInputDispatchTouchEvent
  arg_pInputDispatchTouchEventType
  arg_pInputDispatchTouchEventTouchPoints
  = PInputDispatchTouchEvent
    arg_pInputDispatchTouchEventType
    arg_pInputDispatchTouchEventTouchPoints
    Nothing
    Nothing
instance ToJSON PInputDispatchTouchEvent where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pInputDispatchTouchEventType p),
    ("touchPoints" A..=) <$> Just (pInputDispatchTouchEventTouchPoints p),
    ("modifiers" A..=) <$> (pInputDispatchTouchEventModifiers p),
    ("timestamp" A..=) <$> (pInputDispatchTouchEventTimestamp p)
    ]
instance Command PInputDispatchTouchEvent where
  type CommandResponse PInputDispatchTouchEvent = ()
  commandName _ = "Input.dispatchTouchEvent"
  fromJSON = const . A.Success . const ()

-- | Emulates touch event from the mouse event parameters.

-- | Parameters of the 'Input.emulateTouchFromMouseEvent' command.
data PInputEmulateTouchFromMouseEventType = PInputEmulateTouchFromMouseEventTypeMousePressed | PInputEmulateTouchFromMouseEventTypeMouseReleased | PInputEmulateTouchFromMouseEventTypeMouseMoved | PInputEmulateTouchFromMouseEventTypeMouseWheel
  deriving (Ord, Eq, Show, Read)
instance FromJSON PInputEmulateTouchFromMouseEventType where
  parseJSON = A.withText "PInputEmulateTouchFromMouseEventType" $ \v -> case v of
    "mousePressed" -> pure PInputEmulateTouchFromMouseEventTypeMousePressed
    "mouseReleased" -> pure PInputEmulateTouchFromMouseEventTypeMouseReleased
    "mouseMoved" -> pure PInputEmulateTouchFromMouseEventTypeMouseMoved
    "mouseWheel" -> pure PInputEmulateTouchFromMouseEventTypeMouseWheel
    "_" -> fail "failed to parse PInputEmulateTouchFromMouseEventType"
instance ToJSON PInputEmulateTouchFromMouseEventType where
  toJSON v = A.String $ case v of
    PInputEmulateTouchFromMouseEventTypeMousePressed -> "mousePressed"
    PInputEmulateTouchFromMouseEventTypeMouseReleased -> "mouseReleased"
    PInputEmulateTouchFromMouseEventTypeMouseMoved -> "mouseMoved"
    PInputEmulateTouchFromMouseEventTypeMouseWheel -> "mouseWheel"
data PInputEmulateTouchFromMouseEvent = PInputEmulateTouchFromMouseEvent
  {
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
    --   (default: 0).
    pInputEmulateTouchFromMouseEventModifiers :: Maybe Int,
    -- | Number of times the mouse button was clicked (default: 0).
    pInputEmulateTouchFromMouseEventClickCount :: Maybe Int
  }
  deriving (Eq, Show)
pInputEmulateTouchFromMouseEvent
  -- | Type of the mouse event.
  :: PInputEmulateTouchFromMouseEventType
  -- | X coordinate of the mouse pointer in DIP.
  -> Int
  -- | Y coordinate of the mouse pointer in DIP.
  -> Int
  -- | Mouse button. Only "none", "left", "right" are supported.
  -> InputMouseButton
  -> PInputEmulateTouchFromMouseEvent
pInputEmulateTouchFromMouseEvent
  arg_pInputEmulateTouchFromMouseEventType
  arg_pInputEmulateTouchFromMouseEventX
  arg_pInputEmulateTouchFromMouseEventY
  arg_pInputEmulateTouchFromMouseEventButton
  = PInputEmulateTouchFromMouseEvent
    arg_pInputEmulateTouchFromMouseEventType
    arg_pInputEmulateTouchFromMouseEventX
    arg_pInputEmulateTouchFromMouseEventY
    arg_pInputEmulateTouchFromMouseEventButton
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PInputEmulateTouchFromMouseEvent where
  toJSON p = A.object $ catMaybes [
    ("type" A..=) <$> Just (pInputEmulateTouchFromMouseEventType p),
    ("x" A..=) <$> Just (pInputEmulateTouchFromMouseEventX p),
    ("y" A..=) <$> Just (pInputEmulateTouchFromMouseEventY p),
    ("button" A..=) <$> Just (pInputEmulateTouchFromMouseEventButton p),
    ("timestamp" A..=) <$> (pInputEmulateTouchFromMouseEventTimestamp p),
    ("deltaX" A..=) <$> (pInputEmulateTouchFromMouseEventDeltaX p),
    ("deltaY" A..=) <$> (pInputEmulateTouchFromMouseEventDeltaY p),
    ("modifiers" A..=) <$> (pInputEmulateTouchFromMouseEventModifiers p),
    ("clickCount" A..=) <$> (pInputEmulateTouchFromMouseEventClickCount p)
    ]
instance Command PInputEmulateTouchFromMouseEvent where
  type CommandResponse PInputEmulateTouchFromMouseEvent = ()
  commandName _ = "Input.emulateTouchFromMouseEvent"
  fromJSON = const . A.Success . const ()

-- | Ignores input events (useful while auditing page).

-- | Parameters of the 'Input.setIgnoreInputEvents' command.
data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents
  {
    -- | Ignores input events processing when set to true.
    pInputSetIgnoreInputEventsIgnore :: Bool
  }
  deriving (Eq, Show)
pInputSetIgnoreInputEvents
  -- | Ignores input events processing when set to true.
  :: Bool
  -> PInputSetIgnoreInputEvents
pInputSetIgnoreInputEvents
  arg_pInputSetIgnoreInputEventsIgnore
  = PInputSetIgnoreInputEvents
    arg_pInputSetIgnoreInputEventsIgnore
instance ToJSON PInputSetIgnoreInputEvents where
  toJSON p = A.object $ catMaybes [
    ("ignore" A..=) <$> Just (pInputSetIgnoreInputEventsIgnore p)
    ]
instance Command PInputSetIgnoreInputEvents where
  type CommandResponse PInputSetIgnoreInputEvents = ()
  commandName _ = "Input.setIgnoreInputEvents"
  fromJSON = const . A.Success . const ()

-- | Prevents default drag and drop behavior and instead emits `Input.dragIntercepted` events.
--   Drag and drop behavior can be directly controlled via `Input.dispatchDragEvent`.

-- | Parameters of the 'Input.setInterceptDrags' command.
data PInputSetInterceptDrags = PInputSetInterceptDrags
  {
    pInputSetInterceptDragsEnabled :: Bool
  }
  deriving (Eq, Show)
pInputSetInterceptDrags
  :: Bool
  -> PInputSetInterceptDrags
pInputSetInterceptDrags
  arg_pInputSetInterceptDragsEnabled
  = PInputSetInterceptDrags
    arg_pInputSetInterceptDragsEnabled
instance ToJSON PInputSetInterceptDrags where
  toJSON p = A.object $ catMaybes [
    ("enabled" A..=) <$> Just (pInputSetInterceptDragsEnabled p)
    ]
instance Command PInputSetInterceptDrags where
  type CommandResponse PInputSetInterceptDrags = ()
  commandName _ = "Input.setInterceptDrags"
  fromJSON = const . A.Success . const ()

-- | Synthesizes a pinch gesture over a time period by issuing appropriate touch events.

-- | Parameters of the 'Input.synthesizePinchGesture' command.
data PInputSynthesizePinchGesture = PInputSynthesizePinchGesture
  {
    -- | X coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizePinchGestureX :: Double,
    -- | Y coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizePinchGestureY :: Double,
    -- | Relative scale factor after zooming (>1.0 zooms in, <1.0 zooms out).
    pInputSynthesizePinchGestureScaleFactor :: Double,
    -- | Relative pointer speed in pixels per second (default: 800).
    pInputSynthesizePinchGestureRelativeSpeed :: Maybe Int,
    -- | Which type of input events to be generated (default: 'default', which queries the platform
    --   for the preferred input type).
    pInputSynthesizePinchGestureGestureSourceType :: Maybe InputGestureSourceType
  }
  deriving (Eq, Show)
pInputSynthesizePinchGesture
  -- | X coordinate of the start of the gesture in CSS pixels.
  :: Double
  -- | Y coordinate of the start of the gesture in CSS pixels.
  -> Double
  -- | Relative scale factor after zooming (>1.0 zooms in, <1.0 zooms out).
  -> Double
  -> PInputSynthesizePinchGesture
pInputSynthesizePinchGesture
  arg_pInputSynthesizePinchGestureX
  arg_pInputSynthesizePinchGestureY
  arg_pInputSynthesizePinchGestureScaleFactor
  = PInputSynthesizePinchGesture
    arg_pInputSynthesizePinchGestureX
    arg_pInputSynthesizePinchGestureY
    arg_pInputSynthesizePinchGestureScaleFactor
    Nothing
    Nothing
instance ToJSON PInputSynthesizePinchGesture where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pInputSynthesizePinchGestureX p),
    ("y" A..=) <$> Just (pInputSynthesizePinchGestureY p),
    ("scaleFactor" A..=) <$> Just (pInputSynthesizePinchGestureScaleFactor p),
    ("relativeSpeed" A..=) <$> (pInputSynthesizePinchGestureRelativeSpeed p),
    ("gestureSourceType" A..=) <$> (pInputSynthesizePinchGestureGestureSourceType p)
    ]
instance Command PInputSynthesizePinchGesture where
  type CommandResponse PInputSynthesizePinchGesture = ()
  commandName _ = "Input.synthesizePinchGesture"
  fromJSON = const . A.Success . const ()

-- | Synthesizes a scroll gesture over a time period by issuing appropriate touch events.

-- | Parameters of the 'Input.synthesizeScrollGesture' command.
data PInputSynthesizeScrollGesture = PInputSynthesizeScrollGesture
  {
    -- | X coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizeScrollGestureX :: Double,
    -- | Y coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizeScrollGestureY :: Double,
    -- | The distance to scroll along the X axis (positive to scroll left).
    pInputSynthesizeScrollGestureXDistance :: Maybe Double,
    -- | The distance to scroll along the Y axis (positive to scroll up).
    pInputSynthesizeScrollGestureYDistance :: Maybe Double,
    -- | The number of additional pixels to scroll back along the X axis, in addition to the given
    --   distance.
    pInputSynthesizeScrollGestureXOverscroll :: Maybe Double,
    -- | The number of additional pixels to scroll back along the Y axis, in addition to the given
    --   distance.
    pInputSynthesizeScrollGestureYOverscroll :: Maybe Double,
    -- | Prevent fling (default: true).
    pInputSynthesizeScrollGesturePreventFling :: Maybe Bool,
    -- | Swipe speed in pixels per second (default: 800).
    pInputSynthesizeScrollGestureSpeed :: Maybe Int,
    -- | Which type of input events to be generated (default: 'default', which queries the platform
    --   for the preferred input type).
    pInputSynthesizeScrollGestureGestureSourceType :: Maybe InputGestureSourceType,
    -- | The number of times to repeat the gesture (default: 0).
    pInputSynthesizeScrollGestureRepeatCount :: Maybe Int,
    -- | The number of milliseconds delay between each repeat. (default: 250).
    pInputSynthesizeScrollGestureRepeatDelayMs :: Maybe Int,
    -- | The name of the interaction markers to generate, if not empty (default: "").
    pInputSynthesizeScrollGestureInteractionMarkerName :: Maybe T.Text
  }
  deriving (Eq, Show)
pInputSynthesizeScrollGesture
  -- | X coordinate of the start of the gesture in CSS pixels.
  :: Double
  -- | Y coordinate of the start of the gesture in CSS pixels.
  -> Double
  -> PInputSynthesizeScrollGesture
pInputSynthesizeScrollGesture
  arg_pInputSynthesizeScrollGestureX
  arg_pInputSynthesizeScrollGestureY
  = PInputSynthesizeScrollGesture
    arg_pInputSynthesizeScrollGestureX
    arg_pInputSynthesizeScrollGestureY
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PInputSynthesizeScrollGesture where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pInputSynthesizeScrollGestureX p),
    ("y" A..=) <$> Just (pInputSynthesizeScrollGestureY p),
    ("xDistance" A..=) <$> (pInputSynthesizeScrollGestureXDistance p),
    ("yDistance" A..=) <$> (pInputSynthesizeScrollGestureYDistance p),
    ("xOverscroll" A..=) <$> (pInputSynthesizeScrollGestureXOverscroll p),
    ("yOverscroll" A..=) <$> (pInputSynthesizeScrollGestureYOverscroll p),
    ("preventFling" A..=) <$> (pInputSynthesizeScrollGesturePreventFling p),
    ("speed" A..=) <$> (pInputSynthesizeScrollGestureSpeed p),
    ("gestureSourceType" A..=) <$> (pInputSynthesizeScrollGestureGestureSourceType p),
    ("repeatCount" A..=) <$> (pInputSynthesizeScrollGestureRepeatCount p),
    ("repeatDelayMs" A..=) <$> (pInputSynthesizeScrollGestureRepeatDelayMs p),
    ("interactionMarkerName" A..=) <$> (pInputSynthesizeScrollGestureInteractionMarkerName p)
    ]
instance Command PInputSynthesizeScrollGesture where
  type CommandResponse PInputSynthesizeScrollGesture = ()
  commandName _ = "Input.synthesizeScrollGesture"
  fromJSON = const . A.Success . const ()

-- | Synthesizes a tap gesture over a time period by issuing appropriate touch events.

-- | Parameters of the 'Input.synthesizeTapGesture' command.
data PInputSynthesizeTapGesture = PInputSynthesizeTapGesture
  {
    -- | X coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizeTapGestureX :: Double,
    -- | Y coordinate of the start of the gesture in CSS pixels.
    pInputSynthesizeTapGestureY :: Double,
    -- | Duration between touchdown and touchup events in ms (default: 50).
    pInputSynthesizeTapGestureDuration :: Maybe Int,
    -- | Number of times to perform the tap (e.g. 2 for double tap, default: 1).
    pInputSynthesizeTapGestureTapCount :: Maybe Int,
    -- | Which type of input events to be generated (default: 'default', which queries the platform
    --   for the preferred input type).
    pInputSynthesizeTapGestureGestureSourceType :: Maybe InputGestureSourceType
  }
  deriving (Eq, Show)
pInputSynthesizeTapGesture
  -- | X coordinate of the start of the gesture in CSS pixels.
  :: Double
  -- | Y coordinate of the start of the gesture in CSS pixels.
  -> Double
  -> PInputSynthesizeTapGesture
pInputSynthesizeTapGesture
  arg_pInputSynthesizeTapGestureX
  arg_pInputSynthesizeTapGestureY
  = PInputSynthesizeTapGesture
    arg_pInputSynthesizeTapGestureX
    arg_pInputSynthesizeTapGestureY
    Nothing
    Nothing
    Nothing
instance ToJSON PInputSynthesizeTapGesture where
  toJSON p = A.object $ catMaybes [
    ("x" A..=) <$> Just (pInputSynthesizeTapGestureX p),
    ("y" A..=) <$> Just (pInputSynthesizeTapGestureY p),
    ("duration" A..=) <$> (pInputSynthesizeTapGestureDuration p),
    ("tapCount" A..=) <$> (pInputSynthesizeTapGestureTapCount p),
    ("gestureSourceType" A..=) <$> (pInputSynthesizeTapGestureGestureSourceType p)
    ]
instance Command PInputSynthesizeTapGesture where
  type CommandResponse PInputSynthesizeTapGesture = ()
  commandName _ = "Input.synthesizeTapGesture"
  fromJSON = const . A.Success . const ()

