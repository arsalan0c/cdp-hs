{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Cast :
     A domain for interacting with Cast, Presentation API, and Remote Playback API
     functionalities.

-}


module CDP.Domains.Cast (module CDP.Domains.Cast) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe          
import Data.Functor.Identity
import Data.String
import Data.Text (Text(..))
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



-- | Type 'Cast.Sink'.
data CastSink = CastSink {
  castSinkName :: Text,
  castSinkId :: Text,
  -- | Text describing the current session. Present only if there is an active
  --   session on the sink.
  castSinkSession :: Maybe Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON CastSink  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 , A.omitNothingFields = True}

instance FromJSON  CastSink where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 8 }





-- | Type of the 'Cast.sinksUpdated' event.
data CastSinksUpdated = CastSinksUpdated {
  castSinksUpdatedSinks :: [CastSink]
} deriving (Generic, Eq, Show, Read)
instance ToJSON CastSinksUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  CastSinksUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type of the 'Cast.issueUpdated' event.
data CastIssueUpdated = CastIssueUpdated {
  castIssueUpdatedIssueMessage :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON CastIssueUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  CastIssueUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





-- | Parameters of the 'castEnable' command.
data PCastEnable = PCastEnable {
  pCastEnablePresentationUrl :: Maybe Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PCastEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


-- | Function for the 'Cast.enable' command.
--   Starts observing for sinks that can be used for tab mirroring, and if set,
--   sinks compatible with |presentationUrl| as well. When sinks are found, a
--   |sinksUpdated| event is fired.
--   Also starts observing for issue messages. When an issue is added or removed,
--   an |issueUpdated| event is fired.
--   Parameters: 'PCastEnable'
castEnable :: Handle ev -> PCastEnable -> IO ()
castEnable handle params = sendReceiveCommand handle "Cast.enable" (Just params)


-- | Function for the 'Cast.disable' command.
--   Stops observing for sinks and issues.
castDisable :: Handle ev -> IO ()
castDisable handle = sendReceiveCommand handle "Cast.disable" (Nothing :: Maybe ())


-- | Parameters of the 'castSetSinkToUse' command.
data PCastSetSinkToUse = PCastSetSinkToUse {
  pCastSetSinkToUseSinkName :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastSetSinkToUse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PCastSetSinkToUse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


-- | Function for the 'Cast.setSinkToUse' command.
--   Sets a sink to be used when the web page requests the browser to choose a
--   sink via Presentation API, Remote Playback API, or Cast SDK.
--   Parameters: 'PCastSetSinkToUse'
castSetSinkToUse :: Handle ev -> PCastSetSinkToUse -> IO ()
castSetSinkToUse handle params = sendReceiveCommand handle "Cast.setSinkToUse" (Just params)


-- | Parameters of the 'castStartDesktopMirroring' command.
data PCastStartDesktopMirroring = PCastStartDesktopMirroring {
  pCastStartDesktopMirroringSinkName :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStartDesktopMirroring  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PCastStartDesktopMirroring where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Cast.startDesktopMirroring' command.
--   Starts mirroring the desktop to the sink.
--   Parameters: 'PCastStartDesktopMirroring'
castStartDesktopMirroring :: Handle ev -> PCastStartDesktopMirroring -> IO ()
castStartDesktopMirroring handle params = sendReceiveCommand handle "Cast.startDesktopMirroring" (Just params)


-- | Parameters of the 'castStartTabMirroring' command.
data PCastStartTabMirroring = PCastStartTabMirroring {
  pCastStartTabMirroringSinkName :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStartTabMirroring  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PCastStartTabMirroring where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Cast.startTabMirroring' command.
--   Starts mirroring the tab to the sink.
--   Parameters: 'PCastStartTabMirroring'
castStartTabMirroring :: Handle ev -> PCastStartTabMirroring -> IO ()
castStartTabMirroring handle params = sendReceiveCommand handle "Cast.startTabMirroring" (Just params)


-- | Parameters of the 'castStopCasting' command.
data PCastStopCasting = PCastStopCasting {
  pCastStopCastingSinkName :: Text
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStopCasting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCastStopCasting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


-- | Function for the 'Cast.stopCasting' command.
--   Stops the active Cast session on the sink.
--   Parameters: 'PCastStopCasting'
castStopCasting :: Handle ev -> PCastStopCasting -> IO ()
castStopCasting handle params = sendReceiveCommand handle "Cast.stopCasting" (Just params)



