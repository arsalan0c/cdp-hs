{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= EventBreakpoints

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Sets breakpoint on particular native event.

-- | Parameters of the 'EventBreakpoints.setInstrumentationBreakpoint' command.
data PEventBreakpointsSetInstrumentationBreakpoint = PEventBreakpointsSetInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pEventBreakpointsSetInstrumentationBreakpointEventName :: String
  }
  deriving (Eq, Show)
pEventBreakpointsSetInstrumentationBreakpoint
  -- | Instrumentation name to stop on.
  :: String
  -> PEventBreakpointsSetInstrumentationBreakpoint
pEventBreakpointsSetInstrumentationBreakpoint
  arg_pEventBreakpointsSetInstrumentationBreakpointEventName
  = PEventBreakpointsSetInstrumentationBreakpoint
    arg_pEventBreakpointsSetInstrumentationBreakpointEventName
instance ToJSON PEventBreakpointsSetInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pEventBreakpointsSetInstrumentationBreakpointEventName p)
    ]
instance Command PEventBreakpointsSetInstrumentationBreakpoint where
  type CommandResponse PEventBreakpointsSetInstrumentationBreakpoint = ()
  commandName _ = "EventBreakpoints.setInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

-- | Removes breakpoint on particular native event.

-- | Parameters of the 'EventBreakpoints.removeInstrumentationBreakpoint' command.
data PEventBreakpointsRemoveInstrumentationBreakpoint = PEventBreakpointsRemoveInstrumentationBreakpoint
  {
    -- | Instrumentation name to stop on.
    pEventBreakpointsRemoveInstrumentationBreakpointEventName :: String
  }
  deriving (Eq, Show)
pEventBreakpointsRemoveInstrumentationBreakpoint
  -- | Instrumentation name to stop on.
  :: String
  -> PEventBreakpointsRemoveInstrumentationBreakpoint
pEventBreakpointsRemoveInstrumentationBreakpoint
  arg_pEventBreakpointsRemoveInstrumentationBreakpointEventName
  = PEventBreakpointsRemoveInstrumentationBreakpoint
    arg_pEventBreakpointsRemoveInstrumentationBreakpointEventName
instance ToJSON PEventBreakpointsRemoveInstrumentationBreakpoint where
  toJSON p = A.object $ catMaybes [
    ("eventName" A..=) <$> Just (pEventBreakpointsRemoveInstrumentationBreakpointEventName p)
    ]
instance Command PEventBreakpointsRemoveInstrumentationBreakpoint where
  type CommandResponse PEventBreakpointsRemoveInstrumentationBreakpoint = ()
  commandName _ = "EventBreakpoints.removeInstrumentationBreakpoint"
  fromJSON = const . A.Success . const ()

