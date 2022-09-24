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
   inputTouchPointId :: Maybe Double
} deriving (Generic, Eq, Show, Read)
instance ToJSON InputTouchPoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  InputTouchPoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


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
   pInputDispatchKeyEventLocation :: Maybe Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputDispatchKeyEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PInputDispatchKeyEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


inputDispatchKeyEvent :: Handle ev -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent handle params = sendReceiveCommand handle "Input.dispatchKeyEvent" (Just params)


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



data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
   pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PInputSetIgnoreInputEvents  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PInputSetIgnoreInputEvents where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


inputSetIgnoreInputEvents :: Handle ev -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents handle params = sendReceiveCommand handle "Input.setIgnoreInputEvents" (Just params)



