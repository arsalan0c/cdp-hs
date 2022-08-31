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

module Domains.Target (module Domains.Target) where

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
import qualified Domains.Fetch as Fetch
import qualified Domains.Console as Console
import qualified Domains.Debugger as Debugger
import qualified Domains.Profiler as Profiler
import qualified Domains.Runtime as Runtime
import qualified Domains.Schema as Schema


data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
    targetReceivedMessageFromTargetSessionId :: TargetSessionID,
    targetReceivedMessageFromTargetMessage :: String
} deriving (Eq, Show, Read)
instance FromJSON  TargetReceivedMessageFromTarget where
    parseJSON = A.withObject "TargetReceivedMessageFromTarget" $ \v ->
         TargetReceivedMessageFromTarget <$> v .:  "sessionId"
            <*> v  .:  "message"


instance ToJSON TargetReceivedMessageFromTarget  where
    toJSON v = A.object
        [ "sessionId" .= targetReceivedMessageFromTargetSessionId v
        , "message" .= targetReceivedMessageFromTargetMessage v
        ]


instance FromEvent Event TargetReceivedMessageFromTarget where
    eventName  _ _    =  "Target.receivedMessageFromTarget"
    fromEvent ev =  case ev of EVTargetReceivedMessageFromTarget v -> Just v; _ -> Nothing

data TargetTargetCreated = TargetTargetCreated {
    targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetCreated where
    parseJSON = A.withObject "TargetTargetCreated" $ \v ->
         TargetTargetCreated <$> v .:  "targetInfo"


instance ToJSON TargetTargetCreated  where
    toJSON v = A.object
        [ "targetInfo" .= targetTargetCreatedTargetInfo v
        ]


instance FromEvent Event TargetTargetCreated where
    eventName  _ _    =  "Target.targetCreated"
    fromEvent ev =  case ev of EVTargetTargetCreated v -> Just v; _ -> Nothing

data TargetTargetDestroyed = TargetTargetDestroyed {
    targetTargetDestroyedTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetDestroyed where
    parseJSON = A.withObject "TargetTargetDestroyed" $ \v ->
         TargetTargetDestroyed <$> v .:  "targetId"


instance ToJSON TargetTargetDestroyed  where
    toJSON v = A.object
        [ "targetId" .= targetTargetDestroyedTargetId v
        ]


instance FromEvent Event TargetTargetDestroyed where
    eventName  _ _    =  "Target.targetDestroyed"
    fromEvent ev =  case ev of EVTargetTargetDestroyed v -> Just v; _ -> Nothing

data TargetTargetCrashed = TargetTargetCrashed {
    targetTargetCrashedTargetId :: TargetTargetID,
    targetTargetCrashedStatus :: String,
    targetTargetCrashedErrorCode :: Int
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetCrashed where
    parseJSON = A.withObject "TargetTargetCrashed" $ \v ->
         TargetTargetCrashed <$> v .:  "targetId"
            <*> v  .:  "status"
            <*> v  .:  "errorCode"


instance ToJSON TargetTargetCrashed  where
    toJSON v = A.object
        [ "targetId" .= targetTargetCrashedTargetId v
        , "status" .= targetTargetCrashedStatus v
        , "errorCode" .= targetTargetCrashedErrorCode v
        ]


instance FromEvent Event TargetTargetCrashed where
    eventName  _ _    =  "Target.targetCrashed"
    fromEvent ev =  case ev of EVTargetTargetCrashed v -> Just v; _ -> Nothing

data TargetTargetInfoChanged = TargetTargetInfoChanged {
    targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetInfoChanged where
    parseJSON = A.withObject "TargetTargetInfoChanged" $ \v ->
         TargetTargetInfoChanged <$> v .:  "targetInfo"


instance ToJSON TargetTargetInfoChanged  where
    toJSON v = A.object
        [ "targetInfo" .= targetTargetInfoChangedTargetInfo v
        ]


instance FromEvent Event TargetTargetInfoChanged where
    eventName  _ _    =  "Target.targetInfoChanged"
    fromEvent ev =  case ev of EVTargetTargetInfoChanged v -> Just v; _ -> Nothing



type TargetTargetID = String

type TargetSessionID = String

data TargetTargetInfo = TargetTargetInfo {
    targetTargetInfoTargetId :: TargetTargetID,
    targetTargetInfoType :: String,
    targetTargetInfoTitle :: String,
    targetTargetInfoUrl :: String,
    targetTargetInfoAttached :: Bool,
    targetTargetInfoOpenerId :: Maybe TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetTargetInfo where
    parseJSON = A.withObject "TargetTargetInfo" $ \v ->
         TargetTargetInfo <$> v .:  "targetId"
            <*> v  .:  "type"
            <*> v  .:  "title"
            <*> v  .:  "url"
            <*> v  .:  "attached"
            <*> v  .:?  "openerId"


instance ToJSON TargetTargetInfo  where
    toJSON v = A.object
        [ "targetId" .= targetTargetInfoTargetId v
        , "type" .= targetTargetInfoType v
        , "title" .= targetTargetInfoTitle v
        , "url" .= targetTargetInfoUrl v
        , "attached" .= targetTargetInfoAttached v
        , "openerId" .= targetTargetInfoOpenerId v
        ]





data PTargetActivateTarget = PTargetActivateTarget {
    pTargetActivateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetActivateTarget where
    parseJSON = A.withObject "PTargetActivateTarget" $ \v ->
         PTargetActivateTarget <$> v .:  "targetId"


instance ToJSON PTargetActivateTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetActivateTargetTargetId v
        ]


targetActivateTarget :: Session -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget session params = sendReceiveCommand session "Target.activateTarget" (Just params)

data TargetAttachToTarget = TargetAttachToTarget {
    targetAttachToTargetSessionId :: TargetSessionID
} deriving (Eq, Show, Read)
instance FromJSON  TargetAttachToTarget where
    parseJSON = A.withObject "TargetAttachToTarget" $ \v ->
         TargetAttachToTarget <$> v .:  "sessionId"



instance Command  TargetAttachToTarget where
    commandName _ = "Target.attachToTarget"

data PTargetAttachToTarget = PTargetAttachToTarget {
    pTargetAttachToTargetTargetId :: TargetTargetID,
    pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetAttachToTarget where
    parseJSON = A.withObject "PTargetAttachToTarget" $ \v ->
         PTargetAttachToTarget <$> v .:  "targetId"
            <*> v  .:?  "flatten"


instance ToJSON PTargetAttachToTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetAttachToTargetTargetId v
        , "flatten" .= pTargetAttachToTargetFlatten v
        ]


targetAttachToTarget :: Session -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget session params = sendReceiveCommandResult session "Target.attachToTarget" (Just params)



data PTargetCloseTarget = PTargetCloseTarget {
    pTargetCloseTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetCloseTarget where
    parseJSON = A.withObject "PTargetCloseTarget" $ \v ->
         PTargetCloseTarget <$> v .:  "targetId"


instance ToJSON PTargetCloseTarget  where
    toJSON v = A.object
        [ "targetId" .= pTargetCloseTargetTargetId v
        ]


targetCloseTarget :: Session -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget session params = sendReceiveCommand session "Target.closeTarget" (Just params)

data TargetCreateTarget = TargetCreateTarget {
    targetCreateTargetTargetId :: TargetTargetID
} deriving (Eq, Show, Read)
instance FromJSON  TargetCreateTarget where
    parseJSON = A.withObject "TargetCreateTarget" $ \v ->
         TargetCreateTarget <$> v .:  "targetId"



instance Command  TargetCreateTarget where
    commandName _ = "Target.createTarget"

data PTargetCreateTarget = PTargetCreateTarget {
    pTargetCreateTargetUrl :: String,
    pTargetCreateTargetWidth :: Maybe Int,
    pTargetCreateTargetHeight :: Maybe Int,
    pTargetCreateTargetNewWindow :: Maybe Bool,
    pTargetCreateTargetBackground :: Maybe Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetCreateTarget where
    parseJSON = A.withObject "PTargetCreateTarget" $ \v ->
         PTargetCreateTarget <$> v .:  "url"
            <*> v  .:?  "width"
            <*> v  .:?  "height"
            <*> v  .:?  "newWindow"
            <*> v  .:?  "background"


instance ToJSON PTargetCreateTarget  where
    toJSON v = A.object
        [ "url" .= pTargetCreateTargetUrl v
        , "width" .= pTargetCreateTargetWidth v
        , "height" .= pTargetCreateTargetHeight v
        , "newWindow" .= pTargetCreateTargetNewWindow v
        , "background" .= pTargetCreateTargetBackground v
        ]


targetCreateTarget :: Session -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget session params = sendReceiveCommandResult session "Target.createTarget" (Just params)



data PTargetDetachFromTarget = PTargetDetachFromTarget {
    pTargetDetachFromTargetSessionId :: Maybe TargetSessionID
} deriving (Eq, Show, Read)
instance FromJSON  PTargetDetachFromTarget where
    parseJSON = A.withObject "PTargetDetachFromTarget" $ \v ->
         PTargetDetachFromTarget <$> v .:?  "sessionId"


instance ToJSON PTargetDetachFromTarget  where
    toJSON v = A.object
        [ "sessionId" .= pTargetDetachFromTargetSessionId v
        ]


targetDetachFromTarget :: Session -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget session params = sendReceiveCommand session "Target.detachFromTarget" (Just params)

data TargetGetTargets = TargetGetTargets {
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Eq, Show, Read)
instance FromJSON  TargetGetTargets where
    parseJSON = A.withObject "TargetGetTargets" $ \v ->
         TargetGetTargets <$> v .:  "targetInfos"



instance Command  TargetGetTargets where
    commandName _ = "Target.getTargets"


targetGetTargets :: Session -> IO (Either Error TargetGetTargets)
targetGetTargets session = sendReceiveCommandResult session "Target.getTargets" (Nothing :: Maybe ())



data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
    pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Eq, Show, Read)
instance FromJSON  PTargetSetDiscoverTargets where
    parseJSON = A.withObject "PTargetSetDiscoverTargets" $ \v ->
         PTargetSetDiscoverTargets <$> v .:  "discover"


instance ToJSON PTargetSetDiscoverTargets  where
    toJSON v = A.object
        [ "discover" .= pTargetSetDiscoverTargetsDiscover v
        ]


targetSetDiscoverTargets :: Session -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets session params = sendReceiveCommand session "Target.setDiscoverTargets" (Just params)

