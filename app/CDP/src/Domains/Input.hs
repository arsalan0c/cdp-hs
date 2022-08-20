{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Input (module Domains.Input) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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



import Utils


data TouchPoint = TouchPoint {
    touchPointX :: Int,
    touchPointY :: Int,
    touchPointRadiusX :: Maybe Int,
    touchPointRadiusY :: Maybe Int,
    touchPointRotationAngle :: Maybe Int,
    touchPointForce :: Maybe Int,
    touchPointId :: Maybe Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TouchPoint where
    parseJSON = A.withObject "TouchPoint" $ \v ->
         TouchPoint <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "radiusX"
            <*> v  .:?  "radiusY"
            <*> v  .:?  "rotationAngle"
            <*> v  .:?  "force"
            <*> v  .:?  "id"


instance ToJSON TouchPoint  where
    toJSON v = A.object
        [ "x" .= touchPointX v
        , "y" .= touchPointY v
        , "radiusX" .= touchPointRadiusX v
        , "radiusY" .= touchPointRadiusY v
        , "rotationAngle" .= touchPointRotationAngle v
        , "force" .= touchPointForce v
        , "id" .= touchPointId v
        ]



data MouseButton = MouseButtonNone | MouseButtonLeft | MouseButtonMiddle | MouseButtonRight | MouseButtonBack | MouseButtonForward
    deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON MouseButton where
    parseJSON = A.withText  "MouseButton"  $ \v -> do
        pure $ case v of
                "none" -> MouseButtonNone
                "left" -> MouseButtonLeft
                "middle" -> MouseButtonMiddle
                "right" -> MouseButtonRight
                "back" -> MouseButtonBack
                "forward" -> MouseButtonForward
                _ -> error "failed to parse MouseButton"

instance ToJSON MouseButton where
    toJSON v = A.String $
        case v of
                MouseButtonNone -> "none"
                MouseButtonLeft -> "left"
                MouseButtonMiddle -> "middle"
                MouseButtonRight -> "right"
                MouseButtonBack -> "back"
                MouseButtonForward -> "forward"



type TimeSinceEpoch = Int

dispatchKeyEvent :: Session a -> String -> Maybe Int -> Maybe TimeSinceEpoch -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> IO (Maybe Error)
dispatchKeyEvent session dispatchKeyEventType dispatchKeyEventModifiers dispatchKeyEventTimestamp dispatchKeyEventText dispatchKeyEventUnmodifiedText dispatchKeyEventKeyIdentifier dispatchKeyEventCode dispatchKeyEventKey dispatchKeyEventWindowsVirtualKeyCode dispatchKeyEventNativeVirtualKeyCode dispatchKeyEventAutoRepeat dispatchKeyEventIsKeypad dispatchKeyEventIsSystemKey dispatchKeyEventLocation = sendReceiveCommand (conn session) ("Input","dispatchKeyEvent") ([("type", ToJSONEx dispatchKeyEventType)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) dispatchKeyEventModifiers, fmap (("timestamp",) . ToJSONEx) dispatchKeyEventTimestamp, fmap (("text",) . ToJSONEx) dispatchKeyEventText, fmap (("unmodifiedText",) . ToJSONEx) dispatchKeyEventUnmodifiedText, fmap (("keyIdentifier",) . ToJSONEx) dispatchKeyEventKeyIdentifier, fmap (("code",) . ToJSONEx) dispatchKeyEventCode, fmap (("key",) . ToJSONEx) dispatchKeyEventKey, fmap (("windowsVirtualKeyCode",) . ToJSONEx) dispatchKeyEventWindowsVirtualKeyCode, fmap (("nativeVirtualKeyCode",) . ToJSONEx) dispatchKeyEventNativeVirtualKeyCode, fmap (("autoRepeat",) . ToJSONEx) dispatchKeyEventAutoRepeat, fmap (("isKeypad",) . ToJSONEx) dispatchKeyEventIsKeypad, fmap (("isSystemKey",) . ToJSONEx) dispatchKeyEventIsSystemKey, fmap (("location",) . ToJSONEx) dispatchKeyEventLocation]))


dispatchMouseEvent :: Session a -> String -> Int -> Int -> Maybe Int -> Maybe TimeSinceEpoch -> Maybe MouseButton -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> IO (Maybe Error)
dispatchMouseEvent session dispatchMouseEventType dispatchMouseEventX dispatchMouseEventY dispatchMouseEventModifiers dispatchMouseEventTimestamp dispatchMouseEventButton dispatchMouseEventButtons dispatchMouseEventClickCount dispatchMouseEventDeltaX dispatchMouseEventDeltaY dispatchMouseEventPointerType = sendReceiveCommand (conn session) ("Input","dispatchMouseEvent") ([("type", ToJSONEx dispatchMouseEventType), ("x", ToJSONEx dispatchMouseEventX), ("y", ToJSONEx dispatchMouseEventY)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) dispatchMouseEventModifiers, fmap (("timestamp",) . ToJSONEx) dispatchMouseEventTimestamp, fmap (("button",) . ToJSONEx) dispatchMouseEventButton, fmap (("buttons",) . ToJSONEx) dispatchMouseEventButtons, fmap (("clickCount",) . ToJSONEx) dispatchMouseEventClickCount, fmap (("deltaX",) . ToJSONEx) dispatchMouseEventDeltaX, fmap (("deltaY",) . ToJSONEx) dispatchMouseEventDeltaY, fmap (("pointerType",) . ToJSONEx) dispatchMouseEventPointerType]))


dispatchTouchEvent :: Session a -> String -> [TouchPoint] -> Maybe Int -> Maybe TimeSinceEpoch -> IO (Maybe Error)
dispatchTouchEvent session dispatchTouchEventType dispatchTouchEventTouchPoints dispatchTouchEventModifiers dispatchTouchEventTimestamp = sendReceiveCommand (conn session) ("Input","dispatchTouchEvent") ([("type", ToJSONEx dispatchTouchEventType), ("touchPoints", ToJSONEx dispatchTouchEventTouchPoints)] ++ (catMaybes [fmap (("modifiers",) . ToJSONEx) dispatchTouchEventModifiers, fmap (("timestamp",) . ToJSONEx) dispatchTouchEventTimestamp]))


setIgnoreInputEvents :: Session a -> Bool -> IO (Maybe Error)
setIgnoreInputEvents session setIgnoreInputEventsIgnore = sendReceiveCommand (conn session) ("Input","setIgnoreInputEvents") ([("ignore", ToJSONEx setIgnoreInputEventsIgnore)] ++ (catMaybes []))


