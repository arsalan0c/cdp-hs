{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.Input (module Domains.Input) where

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

import Utils



data InputEvent = 
    deriving (Eq, Show, Read)



subscribe :: forall a. FromEvent InputEvent a => Session -> ( a -> IO () ) -> IO ()
subscribe (Session session') handler1 = subscribe' paev session' name handler2
  where
    handler2 = maybe (pure ()) handler1 . fromEvent
    name     = eventName pev pa
    paev     = Proxy :: Proxy Event
    pev      = Proxy :: Proxy InputEvent
    pa       = Proxy :: Proxy a


data InputTouchPoint = InputTouchPoint {
    inputTouchPointX :: Int,
    inputTouchPointY :: Int,
    inputTouchPointRadiusX :: Maybe Int,
    inputTouchPointRadiusY :: Maybe Int,
    inputTouchPointRotationAngle :: Maybe Int,
    inputTouchPointForce :: Maybe Int,
    inputTouchPointId :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  InputTouchPoint where
    parseJSON = A.withObject "InputTouchPoint" $ \v ->
         InputTouchPoint <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "radiusX"
            <*> v  .:?  "radiusY"
            <*> v  .:?  "rotationAngle"
            <*> v  .:?  "force"
            <*> v  .:?  "id"


instance ToJSON InputTouchPoint  where
    toJSON v = A.object
        [ "x" .= inputTouchPointX v
        , "y" .= inputTouchPointY v
        , "radiusX" .= inputTouchPointRadiusX v
        , "radiusY" .= inputTouchPointRadiusY v
        , "rotationAngle" .= inputTouchPointRotationAngle v
        , "force" .= inputTouchPointForce v
        , "id" .= inputTouchPointId v
        ]



data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
    deriving (Eq, Show, Read)
instance FromJSON InputMouseButton where
    parseJSON = A.withText  "InputMouseButton"  $ \v -> do
        case v of
                "none" -> pure $ InputMouseButtonNone
                "left" -> pure $ InputMouseButtonLeft
                "middle" -> pure $ InputMouseButtonMiddle
                "right" -> pure $ InputMouseButtonRight
                "back" -> pure $ InputMouseButtonBack
                "forward" -> pure $ InputMouseButtonForward
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



type InputTimeSinceEpoch = Int



data PInputDispatchKeyEvent = PInputDispatchKeyEvent {
    pInputDispatchKeyEventType :: String,
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
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchKeyEvent where
    parseJSON = A.withObject "PInputDispatchKeyEvent" $ \v ->
         PInputDispatchKeyEvent <$> v .:  "type"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"
            <*> v  .:?  "text"
            <*> v  .:?  "unmodifiedText"
            <*> v  .:?  "keyIdentifier"
            <*> v  .:?  "code"
            <*> v  .:?  "key"
            <*> v  .:?  "windowsVirtualKeyCode"
            <*> v  .:?  "nativeVirtualKeyCode"
            <*> v  .:?  "autoRepeat"
            <*> v  .:?  "isKeypad"
            <*> v  .:?  "isSystemKey"
            <*> v  .:?  "location"


instance ToJSON PInputDispatchKeyEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchKeyEventType v
        , "modifiers" .= pInputDispatchKeyEventModifiers v
        , "timestamp" .= pInputDispatchKeyEventTimestamp v
        , "text" .= pInputDispatchKeyEventText v
        , "unmodifiedText" .= pInputDispatchKeyEventUnmodifiedText v
        , "keyIdentifier" .= pInputDispatchKeyEventKeyIdentifier v
        , "code" .= pInputDispatchKeyEventCode v
        , "key" .= pInputDispatchKeyEventKey v
        , "windowsVirtualKeyCode" .= pInputDispatchKeyEventWindowsVirtualKeyCode v
        , "nativeVirtualKeyCode" .= pInputDispatchKeyEventNativeVirtualKeyCode v
        , "autoRepeat" .= pInputDispatchKeyEventAutoRepeat v
        , "isKeypad" .= pInputDispatchKeyEventIsKeypad v
        , "isSystemKey" .= pInputDispatchKeyEventIsSystemKey v
        , "location" .= pInputDispatchKeyEventLocation v
        ]


inputDispatchKeyEvent :: Session -> PInputDispatchKeyEvent -> IO (Maybe Error)
inputDispatchKeyEvent session params = sendReceiveCommand session "Input.dispatchKeyEvent" (Just params)



data PInputDispatchMouseEvent = PInputDispatchMouseEvent {
    pInputDispatchMouseEventType :: String,
    pInputDispatchMouseEventX :: Int,
    pInputDispatchMouseEventY :: Int,
    pInputDispatchMouseEventModifiers :: Maybe Int,
    pInputDispatchMouseEventTimestamp :: Maybe InputTimeSinceEpoch,
    pInputDispatchMouseEventButton :: Maybe InputMouseButton,
    pInputDispatchMouseEventButtons :: Maybe Int,
    pInputDispatchMouseEventClickCount :: Maybe Int,
    pInputDispatchMouseEventDeltaX :: Maybe Int,
    pInputDispatchMouseEventDeltaY :: Maybe Int,
    pInputDispatchMouseEventPointerType :: Maybe String
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchMouseEvent where
    parseJSON = A.withObject "PInputDispatchMouseEvent" $ \v ->
         PInputDispatchMouseEvent <$> v .:  "type"
            <*> v  .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"
            <*> v  .:?  "button"
            <*> v  .:?  "buttons"
            <*> v  .:?  "clickCount"
            <*> v  .:?  "deltaX"
            <*> v  .:?  "deltaY"
            <*> v  .:?  "pointerType"


instance ToJSON PInputDispatchMouseEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchMouseEventType v
        , "x" .= pInputDispatchMouseEventX v
        , "y" .= pInputDispatchMouseEventY v
        , "modifiers" .= pInputDispatchMouseEventModifiers v
        , "timestamp" .= pInputDispatchMouseEventTimestamp v
        , "button" .= pInputDispatchMouseEventButton v
        , "buttons" .= pInputDispatchMouseEventButtons v
        , "clickCount" .= pInputDispatchMouseEventClickCount v
        , "deltaX" .= pInputDispatchMouseEventDeltaX v
        , "deltaY" .= pInputDispatchMouseEventDeltaY v
        , "pointerType" .= pInputDispatchMouseEventPointerType v
        ]


inputDispatchMouseEvent :: Session -> PInputDispatchMouseEvent -> IO (Maybe Error)
inputDispatchMouseEvent session params = sendReceiveCommand session "Input.dispatchMouseEvent" (Just params)



data PInputDispatchTouchEvent = PInputDispatchTouchEvent {
    pInputDispatchTouchEventType :: String,
    pInputDispatchTouchEventTouchPoints :: [InputTouchPoint],
    pInputDispatchTouchEventModifiers :: Maybe Int,
    pInputDispatchTouchEventTimestamp :: Maybe InputTimeSinceEpoch
} deriving (Eq, Show, Read)
instance FromJSON  PInputDispatchTouchEvent where
    parseJSON = A.withObject "PInputDispatchTouchEvent" $ \v ->
         PInputDispatchTouchEvent <$> v .:  "type"
            <*> v  .:  "touchPoints"
            <*> v  .:?  "modifiers"
            <*> v  .:?  "timestamp"


instance ToJSON PInputDispatchTouchEvent  where
    toJSON v = A.object
        [ "type" .= pInputDispatchTouchEventType v
        , "touchPoints" .= pInputDispatchTouchEventTouchPoints v
        , "modifiers" .= pInputDispatchTouchEventModifiers v
        , "timestamp" .= pInputDispatchTouchEventTimestamp v
        ]


inputDispatchTouchEvent :: Session -> PInputDispatchTouchEvent -> IO (Maybe Error)
inputDispatchTouchEvent session params = sendReceiveCommand session "Input.dispatchTouchEvent" (Just params)



data PInputSetIgnoreInputEvents = PInputSetIgnoreInputEvents {
    pInputSetIgnoreInputEventsIgnore :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PInputSetIgnoreInputEvents where
    parseJSON = A.withObject "PInputSetIgnoreInputEvents" $ \v ->
         PInputSetIgnoreInputEvents <$> v .:  "ignore"


instance ToJSON PInputSetIgnoreInputEvents  where
    toJSON v = A.object
        [ "ignore" .= pInputSetIgnoreInputEventsIgnore v
        ]


inputSetIgnoreInputEvents :: Session -> PInputSetIgnoreInputEvents -> IO (Maybe Error)
inputSetIgnoreInputEvents session params = sendReceiveCommand session "Input.setIgnoreInputEvents" (Just params)

