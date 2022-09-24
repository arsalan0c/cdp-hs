{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.Target (module CDP.Domains.Target) where

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



type TargetTargetId = String
type TargetSessionId = String

data TargetTargetInfo = TargetTargetInfo {
   targetTargetInfoTargetId :: TargetTargetId,
   targetTargetInfoType :: String,
   targetTargetInfoTitle :: String,
   targetTargetInfoUrl :: String,
   targetTargetInfoAttached :: Bool,
   targetTargetInfoOpenerId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
   targetReceivedMessageFromTargetSessionId :: TargetSessionId,
   targetReceivedMessageFromTargetMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetReceivedMessageFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  TargetReceivedMessageFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



data TargetTargetCreated = TargetTargetCreated {
   targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data TargetTargetDestroyed = TargetTargetDestroyed {
   targetTargetDestroyedTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  TargetTargetDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



data TargetTargetCrashed = TargetTargetCrashed {
   targetTargetCrashedTargetId :: TargetTargetId,
   targetTargetCrashedStatus :: String,
   targetTargetCrashedErrorCode :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCrashed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCrashed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



data TargetTargetInfoChanged = TargetTargetInfoChanged {
   targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfoChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfoChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





data PTargetActivateTarget = PTargetActivateTarget {
   pTargetActivateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetActivateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetActivateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


targetActivateTarget :: Handle ev -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget handle params = sendReceiveCommand handle "Target.activateTarget" (Just params)



data PTargetAttachToTarget = PTargetAttachToTarget {
   pTargetAttachToTargetTargetId :: TargetTargetId,
   pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAttachToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


targetAttachToTarget :: Handle ev -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget handle params = sendReceiveCommandResult handle "Target.attachToTarget" (Just params)

data TargetAttachToTarget = TargetAttachToTarget {
   targetAttachToTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TargetAttachToTarget where
   commandName _ = "Target.attachToTarget"




data PTargetCloseTarget = PTargetCloseTarget {
   pTargetCloseTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCloseTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PTargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


targetCloseTarget :: Handle ev -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget handle params = sendReceiveCommand handle "Target.closeTarget" (Just params)



data PTargetCreateTarget = PTargetCreateTarget {
   pTargetCreateTargetUrl :: String,
   pTargetCreateTargetWidth :: Maybe Int,
   pTargetCreateTargetHeight :: Maybe Int,
   pTargetCreateTargetNewWindow :: Maybe Bool,
   pTargetCreateTargetBackground :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


targetCreateTarget :: Handle ev -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget handle params = sendReceiveCommandResult handle "Target.createTarget" (Just params)

data TargetCreateTarget = TargetCreateTarget {
   targetCreateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command TargetCreateTarget where
   commandName _ = "Target.createTarget"




data PTargetDetachFromTarget = PTargetDetachFromTarget {
   pTargetDetachFromTargetSessionId :: Maybe TargetSessionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDetachFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PTargetDetachFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


targetDetachFromTarget :: Handle ev -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget handle params = sendReceiveCommand handle "Target.detachFromTarget" (Just params)


targetGetTargets :: Handle ev -> IO (Either Error TargetGetTargets)
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())

data TargetGetTargets = TargetGetTargets {
   targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command TargetGetTargets where
   commandName _ = "Target.getTargets"




data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
   pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetDiscoverTargets  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetDiscoverTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


targetSetDiscoverTargets :: Handle ev -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)



