{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}
module Domains.Target (module Domains.Target) where
import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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



import Utils

data ReceivedMessageFromTarget = ReceivedMessageFromTarget {
    receivedMessageFromTargetSessionId :: SessionID,
    receivedMessageFromTargetMessage :: String
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  ReceivedMessageFromTarget where
    parseJSON = A.withObject "ReceivedMessageFromTarget" $ \v ->
         ReceivedMessageFromTarget <$> v .:  "sessionId"
            <*> v  .:  "message"


instance ToJSON ReceivedMessageFromTarget  where
    toJSON v = A.object
        [ "sessionId" .= receivedMessageFromTargetSessionId v
        , "message" .= receivedMessageFromTargetMessage v
        ]


data TargetCreated = TargetCreated {
    targetCreatedTargetInfo :: TargetInfo
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TargetCreated where
    parseJSON = A.withObject "TargetCreated" $ \v ->
         TargetCreated <$> v .:  "targetInfo"


instance ToJSON TargetCreated  where
    toJSON v = A.object
        [ "targetInfo" .= targetCreatedTargetInfo v
        ]


data TargetDestroyed = TargetDestroyed {
    targetDestroyedTargetId :: TargetID
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TargetDestroyed where
    parseJSON = A.withObject "TargetDestroyed" $ \v ->
         TargetDestroyed <$> v .:  "targetId"


instance ToJSON TargetDestroyed  where
    toJSON v = A.object
        [ "targetId" .= targetDestroyedTargetId v
        ]


data TargetCrashed = TargetCrashed {
    targetCrashedTargetId :: TargetID,
    targetCrashedStatus :: String,
    targetCrashedErrorCode :: Int
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TargetCrashed where
    parseJSON = A.withObject "TargetCrashed" $ \v ->
         TargetCrashed <$> v .:  "targetId"
            <*> v  .:  "status"
            <*> v  .:  "errorCode"


instance ToJSON TargetCrashed  where
    toJSON v = A.object
        [ "targetId" .= targetCrashedTargetId v
        , "status" .= targetCrashedStatus v
        , "errorCode" .= targetCrashedErrorCode v
        ]


data TargetInfoChanged = TargetInfoChanged {
    targetInfoChangedTargetInfo :: TargetInfo
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TargetInfoChanged where
    parseJSON = A.withObject "TargetInfoChanged" $ \v ->
         TargetInfoChanged <$> v .:  "targetInfo"


instance ToJSON TargetInfoChanged  where
    toJSON v = A.object
        [ "targetInfo" .= targetInfoChangedTargetInfo v
        ]



type TargetID = String

type SessionID = String

data TargetInfo = TargetInfo {
    targetInfoTargetId :: TargetID,
    targetInfoType :: String,
    targetInfoTitle :: String,
    targetInfoUrl :: String,
    targetInfoAttached :: Bool,
    targetInfoOpenerId :: Maybe TargetID
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  TargetInfo where
    parseJSON = A.withObject "TargetInfo" $ \v ->
         TargetInfo <$> v .:  "targetId"
            <*> v  .:  "type"
            <*> v  .:  "title"
            <*> v  .:  "url"
            <*> v  .:  "attached"
            <*> v  .:?  "openerId"


instance ToJSON TargetInfo  where
    toJSON v = A.object
        [ "targetId" .= targetInfoTargetId v
        , "type" .= targetInfoType v
        , "title" .= targetInfoTitle v
        , "url" .= targetInfoUrl v
        , "attached" .= targetInfoAttached v
        , "openerId" .= targetInfoOpenerId v
        ]



activateTarget :: Session a -> TargetID -> IO (Maybe Error)
activateTarget session activateTargetTargetId = sendReceiveCommand (conn session) ("Target","activateTarget") ([("targetId", ToJSONEx activateTargetTargetId)] ++ (catMaybes []))

data AttachToTarget = AttachToTarget {
    attachToTargetSessionId :: SessionID
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  AttachToTarget where
    parseJSON = A.withObject "AttachToTarget" $ \v ->
         AttachToTarget <$> v .:  "sessionId"



attachToTarget :: Session a -> TargetID -> Maybe Bool -> IO (Either Error AttachToTarget)
attachToTarget session attachToTargetTargetId attachToTargetFlatten = sendReceiveCommandResult (conn session) ("Target","attachToTarget") ([("targetId", ToJSONEx attachToTargetTargetId)] ++ (catMaybes [fmap (("flatten",) . ToJSONEx) attachToTargetFlatten]))


closeTarget :: Session a -> TargetID -> IO (Maybe Error)
closeTarget session closeTargetTargetId = sendReceiveCommand (conn session) ("Target","closeTarget") ([("targetId", ToJSONEx closeTargetTargetId)] ++ (catMaybes []))

data CreateTarget = CreateTarget {
    createTargetTargetId :: TargetID
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  CreateTarget where
    parseJSON = A.withObject "CreateTarget" $ \v ->
         CreateTarget <$> v .:  "targetId"



createTarget :: Session a -> String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> IO (Either Error CreateTarget)
createTarget session createTargetUrl createTargetWidth createTargetHeight createTargetNewWindow createTargetBackground = sendReceiveCommandResult (conn session) ("Target","createTarget") ([("url", ToJSONEx createTargetUrl)] ++ (catMaybes [fmap (("width",) . ToJSONEx) createTargetWidth, fmap (("height",) . ToJSONEx) createTargetHeight, fmap (("newWindow",) . ToJSONEx) createTargetNewWindow, fmap (("background",) . ToJSONEx) createTargetBackground]))


detachFromTarget :: Session a -> Maybe SessionID -> IO (Maybe Error)
detachFromTarget session detachFromTargetSessionId = sendReceiveCommand (conn session) ("Target","detachFromTarget") ([] ++ (catMaybes [fmap (("sessionId",) . ToJSONEx) detachFromTargetSessionId]))

data GetTargets = GetTargets {
    getTargetsTargetInfos :: [TargetInfo]
} deriving (Ord, Eq, Show, Prelude.Read)
instance FromJSON  GetTargets where
    parseJSON = A.withObject "GetTargets" $ \v ->
         GetTargets <$> v .:  "targetInfos"



getTargets :: Session a -> IO (Either Error GetTargets)
getTargets session  = sendReceiveCommandResult (conn session) ("Target","getTargets") ([] ++ (catMaybes []))


setDiscoverTargets :: Session a -> Bool -> IO (Maybe Error)
setDiscoverTargets session setDiscoverTargetsDiscover = sendReceiveCommand (conn session) ("Target","setDiscoverTargets") ([("discover", ToJSONEx setDiscoverTargetsDiscover)] ++ (catMaybes []))


