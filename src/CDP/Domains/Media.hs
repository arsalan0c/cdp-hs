{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Media

This domain allows detailed inspection of media elements
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'Media.PlayerId'.
--   Players will get an ID that is unique within the agent context.
type MediaPlayerId = T.Text

-- | Type 'Media.Timestamp'.
type MediaTimestamp = Double

-- | Type 'Media.PlayerMessage'.
--   Have one type per entry in MediaLogRecord::Type
--   Corresponds to kMessage
data MediaPlayerMessageLevel = MediaPlayerMessageLevelError | MediaPlayerMessageLevelWarning | MediaPlayerMessageLevelInfo | MediaPlayerMessageLevelDebug
  deriving (Ord, Eq, Show, Read)
instance FromJSON MediaPlayerMessageLevel where
  parseJSON = A.withText "MediaPlayerMessageLevel" $ \v -> case v of
    "error" -> pure MediaPlayerMessageLevelError
    "warning" -> pure MediaPlayerMessageLevelWarning
    "info" -> pure MediaPlayerMessageLevelInfo
    "debug" -> pure MediaPlayerMessageLevelDebug
    "_" -> fail "failed to parse MediaPlayerMessageLevel"
instance ToJSON MediaPlayerMessageLevel where
  toJSON v = A.String $ case v of
    MediaPlayerMessageLevelError -> "error"
    MediaPlayerMessageLevelWarning -> "warning"
    MediaPlayerMessageLevelInfo -> "info"
    MediaPlayerMessageLevelDebug -> "debug"
data MediaPlayerMessage = MediaPlayerMessage
  {
    -- | Keep in sync with MediaLogMessageLevel
    --   We are currently keeping the message level 'error' separate from the
    --   PlayerError type because right now they represent different things,
    --   this one being a DVLOG(ERROR) style log message that gets printed
    --   based on what log level is selected in the UI, and the other is a
    --   representation of a media::PipelineStatus object. Soon however we're
    --   going to be moving away from using PipelineStatus for errors and
    --   introducing a new error type which should hopefully let us integrate
    --   the error log level into the PlayerError type.
    mediaPlayerMessageLevel :: MediaPlayerMessageLevel,
    mediaPlayerMessageMessage :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerMessage where
  parseJSON = A.withObject "MediaPlayerMessage" $ \o -> MediaPlayerMessage
    <$> o A..: "level"
    <*> o A..: "message"
instance ToJSON MediaPlayerMessage where
  toJSON p = A.object $ catMaybes [
    ("level" A..=) <$> Just (mediaPlayerMessageLevel p),
    ("message" A..=) <$> Just (mediaPlayerMessageMessage p)
    ]

-- | Type 'Media.PlayerProperty'.
--   Corresponds to kMediaPropertyChange
data MediaPlayerProperty = MediaPlayerProperty
  {
    mediaPlayerPropertyName :: T.Text,
    mediaPlayerPropertyValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerProperty where
  parseJSON = A.withObject "MediaPlayerProperty" $ \o -> MediaPlayerProperty
    <$> o A..: "name"
    <*> o A..: "value"
instance ToJSON MediaPlayerProperty where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (mediaPlayerPropertyName p),
    ("value" A..=) <$> Just (mediaPlayerPropertyValue p)
    ]

-- | Type 'Media.PlayerEvent'.
--   Corresponds to kMediaEventTriggered
data MediaPlayerEvent = MediaPlayerEvent
  {
    mediaPlayerEventTimestamp :: MediaTimestamp,
    mediaPlayerEventValue :: T.Text
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerEvent where
  parseJSON = A.withObject "MediaPlayerEvent" $ \o -> MediaPlayerEvent
    <$> o A..: "timestamp"
    <*> o A..: "value"
instance ToJSON MediaPlayerEvent where
  toJSON p = A.object $ catMaybes [
    ("timestamp" A..=) <$> Just (mediaPlayerEventTimestamp p),
    ("value" A..=) <$> Just (mediaPlayerEventValue p)
    ]

-- | Type 'Media.PlayerErrorSourceLocation'.
--   Represents logged source line numbers reported in an error.
--   NOTE: file and line are from chromium c++ implementation code, not js.
data MediaPlayerErrorSourceLocation = MediaPlayerErrorSourceLocation
  {
    mediaPlayerErrorSourceLocationFile :: T.Text,
    mediaPlayerErrorSourceLocationLine :: Int
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerErrorSourceLocation where
  parseJSON = A.withObject "MediaPlayerErrorSourceLocation" $ \o -> MediaPlayerErrorSourceLocation
    <$> o A..: "file"
    <*> o A..: "line"
instance ToJSON MediaPlayerErrorSourceLocation where
  toJSON p = A.object $ catMaybes [
    ("file" A..=) <$> Just (mediaPlayerErrorSourceLocationFile p),
    ("line" A..=) <$> Just (mediaPlayerErrorSourceLocationLine p)
    ]

-- | Type 'Media.PlayerError'.
--   Corresponds to kMediaError
data MediaPlayerError = MediaPlayerError
  {
    mediaPlayerErrorErrorType :: T.Text,
    -- | Code is the numeric enum entry for a specific set of error codes, such
    --   as PipelineStatusCodes in media/base/pipeline_status.h
    mediaPlayerErrorCode :: Int,
    -- | A trace of where this error was caused / where it passed through.
    mediaPlayerErrorStack :: [MediaPlayerErrorSourceLocation],
    -- | Errors potentially have a root cause error, ie, a DecoderError might be
    --   caused by an WindowsError
    mediaPlayerErrorCause :: [MediaPlayerError],
    -- | Extra data attached to an error, such as an HRESULT, Video Codec, etc.
    mediaPlayerErrorData :: [(T.Text, T.Text)]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerError where
  parseJSON = A.withObject "MediaPlayerError" $ \o -> MediaPlayerError
    <$> o A..: "errorType"
    <*> o A..: "code"
    <*> o A..: "stack"
    <*> o A..: "cause"
    <*> o A..: "data"
instance ToJSON MediaPlayerError where
  toJSON p = A.object $ catMaybes [
    ("errorType" A..=) <$> Just (mediaPlayerErrorErrorType p),
    ("code" A..=) <$> Just (mediaPlayerErrorCode p),
    ("stack" A..=) <$> Just (mediaPlayerErrorStack p),
    ("cause" A..=) <$> Just (mediaPlayerErrorCause p),
    ("data" A..=) <$> Just (mediaPlayerErrorData p)
    ]

-- | Type of the 'Media.playerPropertiesChanged' event.
data MediaPlayerPropertiesChanged = MediaPlayerPropertiesChanged
  {
    mediaPlayerPropertiesChangedPlayerId :: MediaPlayerId,
    mediaPlayerPropertiesChangedProperties :: [MediaPlayerProperty]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerPropertiesChanged where
  parseJSON = A.withObject "MediaPlayerPropertiesChanged" $ \o -> MediaPlayerPropertiesChanged
    <$> o A..: "playerId"
    <*> o A..: "properties"
instance Event MediaPlayerPropertiesChanged where
  eventName _ = "Media.playerPropertiesChanged"

-- | Type of the 'Media.playerEventsAdded' event.
data MediaPlayerEventsAdded = MediaPlayerEventsAdded
  {
    mediaPlayerEventsAddedPlayerId :: MediaPlayerId,
    mediaPlayerEventsAddedEvents :: [MediaPlayerEvent]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerEventsAdded where
  parseJSON = A.withObject "MediaPlayerEventsAdded" $ \o -> MediaPlayerEventsAdded
    <$> o A..: "playerId"
    <*> o A..: "events"
instance Event MediaPlayerEventsAdded where
  eventName _ = "Media.playerEventsAdded"

-- | Type of the 'Media.playerMessagesLogged' event.
data MediaPlayerMessagesLogged = MediaPlayerMessagesLogged
  {
    mediaPlayerMessagesLoggedPlayerId :: MediaPlayerId,
    mediaPlayerMessagesLoggedMessages :: [MediaPlayerMessage]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerMessagesLogged where
  parseJSON = A.withObject "MediaPlayerMessagesLogged" $ \o -> MediaPlayerMessagesLogged
    <$> o A..: "playerId"
    <*> o A..: "messages"
instance Event MediaPlayerMessagesLogged where
  eventName _ = "Media.playerMessagesLogged"

-- | Type of the 'Media.playerErrorsRaised' event.
data MediaPlayerErrorsRaised = MediaPlayerErrorsRaised
  {
    mediaPlayerErrorsRaisedPlayerId :: MediaPlayerId,
    mediaPlayerErrorsRaisedErrors :: [MediaPlayerError]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayerErrorsRaised where
  parseJSON = A.withObject "MediaPlayerErrorsRaised" $ \o -> MediaPlayerErrorsRaised
    <$> o A..: "playerId"
    <*> o A..: "errors"
instance Event MediaPlayerErrorsRaised where
  eventName _ = "Media.playerErrorsRaised"

-- | Type of the 'Media.playersCreated' event.
data MediaPlayersCreated = MediaPlayersCreated
  {
    mediaPlayersCreatedPlayers :: [MediaPlayerId]
  }
  deriving (Eq, Show)
instance FromJSON MediaPlayersCreated where
  parseJSON = A.withObject "MediaPlayersCreated" $ \o -> MediaPlayersCreated
    <$> o A..: "players"
instance Event MediaPlayersCreated where
  eventName _ = "Media.playersCreated"

-- | Enables the Media domain

-- | Parameters of the 'Media.enable' command.
data PMediaEnable = PMediaEnable
  deriving (Eq, Show)
pMediaEnable
  :: PMediaEnable
pMediaEnable
  = PMediaEnable
instance ToJSON PMediaEnable where
  toJSON _ = A.Null
instance Command PMediaEnable where
  type CommandResponse PMediaEnable = ()
  commandName _ = "Media.enable"
  fromJSON = const . A.Success . const ()

-- | Disables the Media domain.

-- | Parameters of the 'Media.disable' command.
data PMediaDisable = PMediaDisable
  deriving (Eq, Show)
pMediaDisable
  :: PMediaDisable
pMediaDisable
  = PMediaDisable
instance ToJSON PMediaDisable where
  toJSON _ = A.Null
instance Command PMediaDisable where
  type CommandResponse PMediaDisable = ()
  commandName _ = "Media.disable"
  fromJSON = const . A.Success . const ()

