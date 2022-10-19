{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Cast

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




-- | Type 'Cast.Sink'.
data CastSink = CastSink
  {
    castSinkName :: String,
    castSinkId :: String,
    -- | Text describing the current session. Present only if there is an active
    --   session on the sink.
    castSinkSession :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON CastSink where
  parseJSON = A.withObject "CastSink" $ \o -> CastSink
    <$> o A..: "name"
    <*> o A..: "id"
    <*> o A..:? "session"
instance ToJSON CastSink where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (castSinkName p),
    ("id" A..=) <$> Just (castSinkId p),
    ("session" A..=) <$> (castSinkSession p)
    ]

-- | Type of the 'Cast.sinksUpdated' event.
data CastSinksUpdated = CastSinksUpdated
  {
    castSinksUpdatedSinks :: [CastSink]
  }
  deriving (Eq, Show)
instance FromJSON CastSinksUpdated where
  parseJSON = A.withObject "CastSinksUpdated" $ \o -> CastSinksUpdated
    <$> o A..: "sinks"
instance Event CastSinksUpdated where
  eventName _ = "Cast.sinksUpdated"

-- | Type of the 'Cast.issueUpdated' event.
data CastIssueUpdated = CastIssueUpdated
  {
    castIssueUpdatedIssueMessage :: String
  }
  deriving (Eq, Show)
instance FromJSON CastIssueUpdated where
  parseJSON = A.withObject "CastIssueUpdated" $ \o -> CastIssueUpdated
    <$> o A..: "issueMessage"
instance Event CastIssueUpdated where
  eventName _ = "Cast.issueUpdated"

-- | Starts observing for sinks that can be used for tab mirroring, and if set,
--   sinks compatible with |presentationUrl| as well. When sinks are found, a
--   |sinksUpdated| event is fired.
--   Also starts observing for issue messages. When an issue is added or removed,
--   an |issueUpdated| event is fired.

-- | Parameters of the 'Cast.enable' command.
data PCastEnable = PCastEnable
  {
    pCastEnablePresentationUrl :: Maybe String
  }
  deriving (Eq, Show)
pCastEnable
  :: PCastEnable
pCastEnable
  = PCastEnable
    Nothing
instance ToJSON PCastEnable where
  toJSON p = A.object $ catMaybes [
    ("presentationUrl" A..=) <$> (pCastEnablePresentationUrl p)
    ]
instance Command PCastEnable where
  type CommandResponse PCastEnable = ()
  commandName _ = "Cast.enable"
  fromJSON = const . A.Success . const ()

-- | Stops observing for sinks and issues.

-- | Parameters of the 'Cast.disable' command.
data PCastDisable = PCastDisable
  deriving (Eq, Show)
pCastDisable
  :: PCastDisable
pCastDisable
  = PCastDisable
instance ToJSON PCastDisable where
  toJSON _ = A.Null
instance Command PCastDisable where
  type CommandResponse PCastDisable = ()
  commandName _ = "Cast.disable"
  fromJSON = const . A.Success . const ()

-- | Sets a sink to be used when the web page requests the browser to choose a
--   sink via Presentation API, Remote Playback API, or Cast SDK.

-- | Parameters of the 'Cast.setSinkToUse' command.
data PCastSetSinkToUse = PCastSetSinkToUse
  {
    pCastSetSinkToUseSinkName :: String
  }
  deriving (Eq, Show)
pCastSetSinkToUse
  :: String
  -> PCastSetSinkToUse
pCastSetSinkToUse
  arg_pCastSetSinkToUseSinkName
  = PCastSetSinkToUse
    arg_pCastSetSinkToUseSinkName
instance ToJSON PCastSetSinkToUse where
  toJSON p = A.object $ catMaybes [
    ("sinkName" A..=) <$> Just (pCastSetSinkToUseSinkName p)
    ]
instance Command PCastSetSinkToUse where
  type CommandResponse PCastSetSinkToUse = ()
  commandName _ = "Cast.setSinkToUse"
  fromJSON = const . A.Success . const ()

-- | Starts mirroring the desktop to the sink.

-- | Parameters of the 'Cast.startDesktopMirroring' command.
data PCastStartDesktopMirroring = PCastStartDesktopMirroring
  {
    pCastStartDesktopMirroringSinkName :: String
  }
  deriving (Eq, Show)
pCastStartDesktopMirroring
  :: String
  -> PCastStartDesktopMirroring
pCastStartDesktopMirroring
  arg_pCastStartDesktopMirroringSinkName
  = PCastStartDesktopMirroring
    arg_pCastStartDesktopMirroringSinkName
instance ToJSON PCastStartDesktopMirroring where
  toJSON p = A.object $ catMaybes [
    ("sinkName" A..=) <$> Just (pCastStartDesktopMirroringSinkName p)
    ]
instance Command PCastStartDesktopMirroring where
  type CommandResponse PCastStartDesktopMirroring = ()
  commandName _ = "Cast.startDesktopMirroring"
  fromJSON = const . A.Success . const ()

-- | Starts mirroring the tab to the sink.

-- | Parameters of the 'Cast.startTabMirroring' command.
data PCastStartTabMirroring = PCastStartTabMirroring
  {
    pCastStartTabMirroringSinkName :: String
  }
  deriving (Eq, Show)
pCastStartTabMirroring
  :: String
  -> PCastStartTabMirroring
pCastStartTabMirroring
  arg_pCastStartTabMirroringSinkName
  = PCastStartTabMirroring
    arg_pCastStartTabMirroringSinkName
instance ToJSON PCastStartTabMirroring where
  toJSON p = A.object $ catMaybes [
    ("sinkName" A..=) <$> Just (pCastStartTabMirroringSinkName p)
    ]
instance Command PCastStartTabMirroring where
  type CommandResponse PCastStartTabMirroring = ()
  commandName _ = "Cast.startTabMirroring"
  fromJSON = const . A.Success . const ()

-- | Stops the active Cast session on the sink.

-- | Parameters of the 'Cast.stopCasting' command.
data PCastStopCasting = PCastStopCasting
  {
    pCastStopCastingSinkName :: String
  }
  deriving (Eq, Show)
pCastStopCasting
  :: String
  -> PCastStopCasting
pCastStopCasting
  arg_pCastStopCastingSinkName
  = PCastStopCasting
    arg_pCastStopCastingSinkName
instance ToJSON PCastStopCasting where
  toJSON p = A.object $ catMaybes [
    ("sinkName" A..=) <$> Just (pCastStopCastingSinkName p)
    ]
instance Command PCastStopCasting where
  type CommandResponse PCastStopCasting = ()
  commandName _ = "Cast.stopCasting"
  fromJSON = const . A.Success . const ()

