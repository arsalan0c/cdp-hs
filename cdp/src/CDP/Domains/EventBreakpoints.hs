{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  EventBreakpoints :
     EventBreakpoints permits setting breakpoints on particular operations and
     events in targets that run JavaScript but do not have a DOM.
     JavaScript execution will stop on these operations as if there was a regular
     breakpoint set.

-}


module CDP.Domains.EventBreakpoints (module CDP.Domains.EventBreakpoints) where

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







-- | Parameters of the 'eventBreakpointsSetInstrumentationBreakpoint' command.
data PEventBreakpointsSetInstrumentationBreakpoint = PEventBreakpointsSetInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pEventBreakpointsSetInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEventBreakpointsSetInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 , A.omitNothingFields = True}

instance FromJSON  PEventBreakpointsSetInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 45 }


-- | Function for the 'EventBreakpoints.setInstrumentationBreakpoint' command.
 -- Sets breakpoint on particular native event.
-- Parameters: 'PEventBreakpointsSetInstrumentationBreakpoint'
eventBreakpointsSetInstrumentationBreakpoint :: Handle ev -> PEventBreakpointsSetInstrumentationBreakpoint -> IO ()
eventBreakpointsSetInstrumentationBreakpoint handle params = sendReceiveCommand handle "EventBreakpoints.setInstrumentationBreakpoint" (Just params)


-- | Parameters of the 'eventBreakpointsRemoveInstrumentationBreakpoint' command.
data PEventBreakpointsRemoveInstrumentationBreakpoint = PEventBreakpointsRemoveInstrumentationBreakpoint {
  -- | Instrumentation name to stop on.
  pEventBreakpointsRemoveInstrumentationBreakpointEventName :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PEventBreakpointsRemoveInstrumentationBreakpoint  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 48 , A.omitNothingFields = True}

instance FromJSON  PEventBreakpointsRemoveInstrumentationBreakpoint where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 48 }


-- | Function for the 'EventBreakpoints.removeInstrumentationBreakpoint' command.
 -- Removes breakpoint on particular native event.
-- Parameters: 'PEventBreakpointsRemoveInstrumentationBreakpoint'
eventBreakpointsRemoveInstrumentationBreakpoint :: Handle ev -> PEventBreakpointsRemoveInstrumentationBreakpoint -> IO ()
eventBreakpointsRemoveInstrumentationBreakpoint handle params = sendReceiveCommand handle "EventBreakpoints.removeInstrumentationBreakpoint" (Just params)



