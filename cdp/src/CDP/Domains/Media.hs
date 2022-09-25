{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Media (module CDP.Domains.Media) where

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



type MediaPlayerId = String
type MediaTimestamp = Double
data MediaPlayerMessageLevel = MediaPlayerMessageLevelError | MediaPlayerMessageLevelWarning | MediaPlayerMessageLevelInfo | MediaPlayerMessageLevelDebug
   deriving (Ord, Eq, Show, Read)
instance FromJSON MediaPlayerMessageLevel where
   parseJSON = A.withText  "MediaPlayerMessageLevel"  $ \v -> do
      case v of
         "error" -> pure MediaPlayerMessageLevelError
         "warning" -> pure MediaPlayerMessageLevelWarning
         "info" -> pure MediaPlayerMessageLevelInfo
         "debug" -> pure MediaPlayerMessageLevelDebug
         _ -> fail "failed to parse MediaPlayerMessageLevel"

instance ToJSON MediaPlayerMessageLevel where
   toJSON v = A.String $
      case v of
         MediaPlayerMessageLevelError -> "error"
         MediaPlayerMessageLevelWarning -> "warning"
         MediaPlayerMessageLevelInfo -> "info"
         MediaPlayerMessageLevelDebug -> "debug"



data MediaPlayerMessage = MediaPlayerMessage {
   mediaPlayerMessageLevel :: MediaPlayerMessageLevel,
   mediaPlayerMessageMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



data MediaPlayerProperty = MediaPlayerProperty {
   mediaPlayerPropertyName :: String,
   mediaPlayerPropertyValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data MediaPlayerEvent = MediaPlayerEvent {
   mediaPlayerEventTimestamp :: MediaTimestamp,
   mediaPlayerEventValue :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data MediaPlayerErrorSourceLocation = MediaPlayerErrorSourceLocation {
   mediaPlayerErrorSourceLocationFile :: String,
   mediaPlayerErrorSourceLocationLine :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerErrorSourceLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerErrorSourceLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



data MediaPlayerError = MediaPlayerError {
   mediaPlayerErrorErrorType :: String,
   mediaPlayerErrorCode :: Int,
   mediaPlayerErrorStack :: [MediaPlayerErrorSourceLocation],
   mediaPlayerErrorCause :: [MediaPlayerError],
   mediaPlayerErrorData :: [(String, String)]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





data MediaPlayerPropertiesChanged = MediaPlayerPropertiesChanged {
   mediaPlayerPropertiesChangedPlayerId :: MediaPlayerId,
   mediaPlayerPropertiesChangedProperties :: [MediaPlayerProperty]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerPropertiesChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerPropertiesChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



data MediaPlayerEventsAdded = MediaPlayerEventsAdded {
   mediaPlayerEventsAddedPlayerId :: MediaPlayerId,
   mediaPlayerEventsAddedEvents :: [MediaPlayerEvent]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerEventsAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerEventsAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data MediaPlayerMessagesLogged = MediaPlayerMessagesLogged {
   mediaPlayerMessagesLoggedPlayerId :: MediaPlayerId,
   mediaPlayerMessagesLoggedMessages :: [MediaPlayerMessage]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerMessagesLogged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerMessagesLogged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



data MediaPlayerErrorsRaised = MediaPlayerErrorsRaised {
   mediaPlayerErrorsRaisedPlayerId :: MediaPlayerId,
   mediaPlayerErrorsRaisedErrors :: [MediaPlayerError]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerErrorsRaised  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerErrorsRaised where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



data MediaPlayersCreated = MediaPlayersCreated {
   mediaPlayersCreatedPlayers :: [MediaPlayerId]
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayersCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  MediaPlayersCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }




mediaEnable :: Handle ev -> IO (Maybe Error)
mediaEnable handle = sendReceiveCommand handle "Media.enable" (Nothing :: Maybe ())


mediaDisable :: Handle ev -> IO (Maybe Error)
mediaDisable handle = sendReceiveCommand handle "Media.disable" (Nothing :: Maybe ())



