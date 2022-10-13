{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


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
data CastSink = CastSink {
  castSinkName :: String,
  castSinkId :: String,
  -- | Text describing the current session. Present only if there is an active
  --   session on the sink.
  castSinkSession :: Maybe String
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


instance Event CastSinksUpdated where
    eventName _ = "Cast.sinksUpdated"

-- | Type of the 'Cast.issueUpdated' event.
data CastIssueUpdated = CastIssueUpdated {
  castIssueUpdatedIssueMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON CastIssueUpdated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  CastIssueUpdated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Event CastIssueUpdated where
    eventName _ = "Cast.issueUpdated"



-- | Cast.enable
--   Starts observing for sinks that can be used for tab mirroring, and if set,
--   sinks compatible with |presentationUrl| as well. When sinks are found, a
--   |sinksUpdated| event is fired.
--   Also starts observing for issue messages. When an issue is added or removed,
--   an |issueUpdated| event is fired.

-- | Parameters of the 'Cast.enable' command.
data PCastEnable = PCastEnable {
  pCastEnablePresentationUrl :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 , A.omitNothingFields = True}

instance FromJSON  PCastEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 11 }


instance Command PCastEnable where
   type CommandResponse PCastEnable = ()
   commandName _ = "Cast.enable"
   fromJSON = const . A.Success . const ()


-- | Cast.disable
--   Stops observing for sinks and issues.

-- | Parameters of the 'Cast.disable' command.
data PCastDisable = PCastDisable
instance ToJSON PCastDisable where toJSON _ = A.Null

instance Command PCastDisable where
   type CommandResponse PCastDisable = ()
   commandName _ = "Cast.disable"
   fromJSON = const . A.Success . const ()


-- | Cast.setSinkToUse
--   Sets a sink to be used when the web page requests the browser to choose a
--   sink via Presentation API, Remote Playback API, or Cast SDK.

-- | Parameters of the 'Cast.setSinkToUse' command.
data PCastSetSinkToUse = PCastSetSinkToUse {
  pCastSetSinkToUseSinkName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastSetSinkToUse  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  PCastSetSinkToUse where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }


instance Command PCastSetSinkToUse where
   type CommandResponse PCastSetSinkToUse = ()
   commandName _ = "Cast.setSinkToUse"
   fromJSON = const . A.Success . const ()


-- | Cast.startDesktopMirroring
--   Starts mirroring the desktop to the sink.

-- | Parameters of the 'Cast.startDesktopMirroring' command.
data PCastStartDesktopMirroring = PCastStartDesktopMirroring {
  pCastStartDesktopMirroringSinkName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStartDesktopMirroring  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PCastStartDesktopMirroring where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


instance Command PCastStartDesktopMirroring where
   type CommandResponse PCastStartDesktopMirroring = ()
   commandName _ = "Cast.startDesktopMirroring"
   fromJSON = const . A.Success . const ()


-- | Cast.startTabMirroring
--   Starts mirroring the tab to the sink.

-- | Parameters of the 'Cast.startTabMirroring' command.
data PCastStartTabMirroring = PCastStartTabMirroring {
  pCastStartTabMirroringSinkName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStartTabMirroring  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PCastStartTabMirroring where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Command PCastStartTabMirroring where
   type CommandResponse PCastStartTabMirroring = ()
   commandName _ = "Cast.startTabMirroring"
   fromJSON = const . A.Success . const ()


-- | Cast.stopCasting
--   Stops the active Cast session on the sink.

-- | Parameters of the 'Cast.stopCasting' command.
data PCastStopCasting = PCastStopCasting {
  pCastStopCastingSinkName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PCastStopCasting  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  PCastStopCasting where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }


instance Command PCastStopCasting where
   type CommandResponse PCastStopCasting = ()
   commandName _ = "Cast.stopCasting"
   fromJSON = const . A.Success . const ()



