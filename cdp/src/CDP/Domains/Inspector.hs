{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Inspector 
-}


module CDP.Domains.Inspector (module CDP.Domains.Inspector) where

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





-- | Type of the 'Inspector.detached' event.
data InspectorDetached = InspectorDetached {
  -- | The reason why connection has been terminated.
  inspectorDetachedReason :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON InspectorDetached  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 , A.omitNothingFields = True}

instance FromJSON  InspectorDetached where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }



-- | Type of the 'Inspector.targetCrashed' event.
data InspectorTargetCrashed = InspectorTargetCrashed
   deriving (Eq, Show, Read)
instance FromJSON InspectorTargetCrashed where
   parseJSON = A.withText  "InspectorTargetCrashed"  $ \v -> do
      case v of
         "InspectorTargetCrashed" -> pure InspectorTargetCrashed
         _ -> fail "failed to parse InspectorTargetCrashed"



-- | Type of the 'Inspector.targetReloadedAfterCrash' event.
data InspectorTargetReloadedAfterCrash = InspectorTargetReloadedAfterCrash
   deriving (Eq, Show, Read)
instance FromJSON InspectorTargetReloadedAfterCrash where
   parseJSON = A.withText  "InspectorTargetReloadedAfterCrash"  $ \v -> do
      case v of
         "InspectorTargetReloadedAfterCrash" -> pure InspectorTargetReloadedAfterCrash
         _ -> fail "failed to parse InspectorTargetReloadedAfterCrash"





-- | Function for the 'Inspector.disable' command.
-- Disables inspector domain notifications.
inspectorDisable :: Handle ev -> IO (Maybe Error)
inspectorDisable handle = sendReceiveCommand handle "Inspector.disable" (Nothing :: Maybe ())


-- | Function for the 'Inspector.enable' command.
-- Enables inspector domain notifications.
inspectorEnable :: Handle ev -> IO (Maybe Error)
inspectorEnable handle = sendReceiveCommand handle "Inspector.enable" (Nothing :: Maybe ())



