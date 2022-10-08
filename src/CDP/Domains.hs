{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains
( module CDP.Domains.Accessibility
, module CDP.Domains.Animation
, module CDP.Domains.Audits
, module CDP.Domains.BackgroundService
, module CDP.Domains.BrowserTarget
, module CDP.Domains.CSS
, module CDP.Domains.CacheStorage
, module CDP.Domains.Cast
, module CDP.Domains.DOMDebugger
, module CDP.Domains.DOMPageNetworkEmulationSecurity
, module CDP.Domains.DOMSnapshot
, module CDP.Domains.DOMStorage
, module CDP.Domains.Database
, module CDP.Domains.Debugger
, module CDP.Domains.DeviceOrientation
, module CDP.Domains.EventBreakpoints
, module CDP.Domains.Fetch
, module CDP.Domains.HeadlessExperimental
, module CDP.Domains.HeapProfiler
, module CDP.Domains.IO
, module CDP.Domains.IndexedDB
, module CDP.Domains.Input
, module CDP.Domains.Inspector
, module CDP.Domains.LayerTree
, module CDP.Domains.Log
, module CDP.Domains.Media
, module CDP.Domains.Memory
, module CDP.Domains.Overlay
, module CDP.Domains.Performance
, module CDP.Domains.PerformanceTimeline
, module CDP.Domains.Profiler
, module CDP.Domains.Runtime
, module CDP.Domains.ServiceWorker
, module CDP.Domains.Storage
, module CDP.Domains.SystemInfo
, module CDP.Domains.Tethering
, module CDP.Domains.Tracing
, module CDP.Domains.WebAudio
, module CDP.Domains.WebAuthn
) where

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

import CDP.Domains.Accessibility
import CDP.Domains.Accessibility as Accessibility
import CDP.Domains.Animation
import CDP.Domains.Animation as Animation
import CDP.Domains.Audits
import CDP.Domains.Audits as Audits
import CDP.Domains.BackgroundService
import CDP.Domains.BackgroundService as BackgroundService
import CDP.Domains.BrowserTarget
import CDP.Domains.BrowserTarget as BrowserTarget
import CDP.Domains.CSS
import CDP.Domains.CSS as CSS
import CDP.Domains.CacheStorage
import CDP.Domains.CacheStorage as CacheStorage
import CDP.Domains.Cast
import CDP.Domains.Cast as Cast
import CDP.Domains.DOMDebugger
import CDP.Domains.DOMDebugger as DOMDebugger
import CDP.Domains.DOMPageNetworkEmulationSecurity
import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity
import CDP.Domains.DOMSnapshot
import CDP.Domains.DOMSnapshot as DOMSnapshot
import CDP.Domains.DOMStorage
import CDP.Domains.DOMStorage as DOMStorage
import CDP.Domains.Database
import CDP.Domains.Database as Database
import CDP.Domains.Debugger
import CDP.Domains.Debugger as Debugger
import CDP.Domains.DeviceOrientation
import CDP.Domains.DeviceOrientation as DeviceOrientation
import CDP.Domains.EventBreakpoints
import CDP.Domains.EventBreakpoints as EventBreakpoints
import CDP.Domains.Fetch
import CDP.Domains.Fetch as Fetch
import CDP.Domains.HeadlessExperimental
import CDP.Domains.HeadlessExperimental as HeadlessExperimental
import CDP.Domains.HeapProfiler
import CDP.Domains.HeapProfiler as HeapProfiler
import CDP.Domains.IO
import CDP.Domains.IO as IO
import CDP.Domains.IndexedDB
import CDP.Domains.IndexedDB as IndexedDB
import CDP.Domains.Input
import CDP.Domains.Input as Input
import CDP.Domains.Inspector
import CDP.Domains.Inspector as Inspector
import CDP.Domains.LayerTree
import CDP.Domains.LayerTree as LayerTree
import CDP.Domains.Log
import CDP.Domains.Log as Log
import CDP.Domains.Media
import CDP.Domains.Media as Media
import CDP.Domains.Memory
import CDP.Domains.Memory as Memory
import CDP.Domains.Overlay
import CDP.Domains.Overlay as Overlay
import CDP.Domains.Performance
import CDP.Domains.Performance as Performance
import CDP.Domains.PerformanceTimeline
import CDP.Domains.PerformanceTimeline as PerformanceTimeline
import CDP.Domains.Profiler
import CDP.Domains.Profiler as Profiler
import CDP.Domains.Runtime
import CDP.Domains.Runtime as Runtime
import CDP.Domains.ServiceWorker
import CDP.Domains.ServiceWorker as ServiceWorker
import CDP.Domains.Storage
import CDP.Domains.Storage as Storage
import CDP.Domains.SystemInfo
import CDP.Domains.SystemInfo as SystemInfo
import CDP.Domains.Tethering
import CDP.Domains.Tethering as Tethering
import CDP.Domains.Tracing
import CDP.Domains.Tracing as Tracing
import CDP.Domains.WebAudio
import CDP.Domains.WebAudio as WebAudio
import CDP.Domains.WebAuthn
import CDP.Domains.WebAuthn as WebAuthn

