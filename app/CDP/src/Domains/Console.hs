{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Domains.Console (module Domains.Console) where

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

import Utils

import qualified Domains.Browser as Browser
import qualified Domains.DOM as DOM
import qualified Domains.DOMDebugger as DOMDebugger
import qualified Domains.Emulation as Emulation
import qualified Domains.IO as IO
import qualified Domains.Input as Input
import qualified Domains.Log as Log
import qualified Domains.Network as Network
import qualified Domains.Page as Page
import qualified Domains.Performance as Performance
import qualified Domains.Security as Security
import qualified Domains.Target as Target
import qualified Domains.Fetch as Fetch
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


data ConsoleMessageAdded = ConsoleMessageAdded {
    consoleMessageAddedMessage :: ConsoleConsoleMessage
} deriving (Eq, Show, Read)
instance FromJSON  ConsoleMessageAdded where
    parseJSON = A.withObject "ConsoleMessageAdded" $ \v ->
         ConsoleMessageAdded <$> v .:  "message"


instance ToJSON ConsoleMessageAdded  where
    toJSON v = A.object
        [ "message" .= consoleMessageAddedMessage v
        ]


instance FromEvent Event ConsoleMessageAdded where
    eventName  _ _    =  "Console.messageAdded"
    fromEvent ev =  case ev of EVConsoleMessageAdded v -> Just v; _ -> Nothing



data ConsoleConsoleMessage = ConsoleConsoleMessage {
    consoleConsoleMessageSource :: String,
    consoleConsoleMessageLevel :: String,
    consoleConsoleMessageText :: String,
    consoleConsoleMessageUrl :: Maybe String,
    consoleConsoleMessageLine :: Maybe Int,
    consoleConsoleMessageColumn :: Maybe Int
} deriving (Eq, Show, Read)
instance FromJSON  ConsoleConsoleMessage where
    parseJSON = A.withObject "ConsoleConsoleMessage" $ \v ->
         ConsoleConsoleMessage <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:?  "url"
            <*> v  .:?  "line"
            <*> v  .:?  "column"


instance ToJSON ConsoleConsoleMessage  where
    toJSON v = A.object
        [ "source" .= consoleConsoleMessageSource v
        , "level" .= consoleConsoleMessageLevel v
        , "text" .= consoleConsoleMessageText v
        , "url" .= consoleConsoleMessageUrl v
        , "line" .= consoleConsoleMessageLine v
        , "column" .= consoleConsoleMessageColumn v
        ]






consoleClearMessages :: Session -> IO (Maybe Error)
consoleClearMessages session = sendReceiveCommand session "Console.clearMessages" (Nothing :: Maybe ())




consoleDisable :: Session -> IO (Maybe Error)
consoleDisable session = sendReceiveCommand session "Console.disable" (Nothing :: Maybe ())




consoleEnable :: Session -> IO (Maybe Error)
consoleEnable session = sendReceiveCommand session "Console.enable" (Nothing :: Maybe ())

