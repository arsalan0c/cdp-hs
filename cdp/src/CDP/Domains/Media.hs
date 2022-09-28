{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Media :
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



-- | Players will get an ID that is unique within the agent context.
type MediaPlayerId = String

-- | Type 'Media.Timestamp' .
type MediaTimestamp = Double

-- | Have one type per entry in MediaLogRecord::Type
-- Corresponds to kMessage
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
   mediaPlayerMessageLevel :: MediaPlayerMessageLevel, -- ^ Keep in sync with MediaLogMessageLevel
We are currently keeping the message level 'error' separate from the
PlayerError type because right now they represent different things,
this one being a DVLOG(ERROR) style log message that gets printed
based on what log level is selected in the UI, and the other is a
representation of a media::PipelineStatus object. Soon however we're
going to be moving away from using PipelineStatus for errors and
introducing a new error type which should hopefully let us integrate
the error log level into the PlayerError type.

} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerMessage  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerMessage where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }



-- | Corresponds to kMediaPropertyChange
data MediaPlayerProperty = MediaPlayerProperty {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerProperty  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerProperty where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Corresponds to kMediaEventTriggered
data MediaPlayerEvent = MediaPlayerEvent {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerEvent  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerEvent where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Represents logged source line numbers reported in an error.
-- NOTE: file and line are from chromium c++ implementation code, not js.
data MediaPlayerErrorSourceLocation = MediaPlayerErrorSourceLocation {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerErrorSourceLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerErrorSourceLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 30 }



-- | Corresponds to kMediaError
data MediaPlayerError = MediaPlayerError {

   mediaPlayerErrorCode :: MediaPlayerErrorCode, -- ^ Code is the numeric enum entry for a specific set of error codes, such
as PipelineStatusCodes in media/base/pipeline_status.h
   mediaPlayerErrorStack :: MediaPlayerErrorStack, -- ^ A trace of where this error was caused / where it passed through.
   mediaPlayerErrorCause :: MediaPlayerErrorCause, -- ^ Errors potentially have a root cause error, ie, a DecoderError might be
caused by an WindowsError
   mediaPlayerErrorData :: MediaPlayerErrorData -- ^ Extra data attached to an error, such as an HRESULT, Video Codec, etc.
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerError  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerError where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





-- | Type of the 'Media.playerPropertiesChanged' event.
data MediaPlayerPropertiesChanged = MediaPlayerPropertiesChanged {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerPropertiesChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerPropertiesChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }



-- | Type of the 'Media.playerEventsAdded' event.
data MediaPlayerEventsAdded = MediaPlayerEventsAdded {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerEventsAdded  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerEventsAdded where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Media.playerMessagesLogged' event.
data MediaPlayerMessagesLogged = MediaPlayerMessagesLogged {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerMessagesLogged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerMessagesLogged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }



-- | Type of the 'Media.playerErrorsRaised' event.
data MediaPlayerErrorsRaised = MediaPlayerErrorsRaised {


} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayerErrorsRaised  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  MediaPlayerErrorsRaised where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }



-- | Type of the 'Media.playersCreated' event.
data MediaPlayersCreated = MediaPlayersCreated {
} deriving (Generic, Eq, Show, Read)
instance ToJSON MediaPlayersCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  MediaPlayersCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }





-- | Function for the command 'Media.enable'.
-- Enables the Media domain
mediaEnable :: Handle ev -> IO (Maybe Error)
mediaEnable handle = sendReceiveCommand handle "Media.enable" (Nothing :: Maybe ())


-- | Function for the command 'Media.disable'.
-- Disables the Media domain.
mediaDisable :: Handle ev -> IO (Maybe Error)
mediaDisable handle = sendReceiveCommand handle "Media.disable" (Nothing :: Maybe ())



