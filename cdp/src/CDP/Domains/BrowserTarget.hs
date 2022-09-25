{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.BrowserTarget (module CDP.Domains.BrowserTarget) where

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

import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


type BrowserBrowserContextId = String
type BrowserWindowId = Int
data BrowserWindowState = BrowserWindowStateNormal | BrowserWindowStateMinimized | BrowserWindowStateMaximized | BrowserWindowStateFullscreen
   deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserWindowState where
   parseJSON = A.withText  "BrowserWindowState"  $ \v -> do
      case v of
         "normal" -> pure BrowserWindowStateNormal
         "minimized" -> pure BrowserWindowStateMinimized
         "maximized" -> pure BrowserWindowStateMaximized
         "fullscreen" -> pure BrowserWindowStateFullscreen
         _ -> fail "failed to parse BrowserWindowState"

instance ToJSON BrowserWindowState where
   toJSON v = A.String $
      case v of
         BrowserWindowStateNormal -> "normal"
         BrowserWindowStateMinimized -> "minimized"
         BrowserWindowStateMaximized -> "maximized"
         BrowserWindowStateFullscreen -> "fullscreen"



data BrowserBounds = BrowserBounds {
   browserBoundsLeft :: Maybe Int,
   browserBoundsTop :: Maybe Int,
   browserBoundsWidth :: Maybe Int,
   browserBoundsHeight :: Maybe Int,
   browserBoundsWindowState :: Maybe BrowserWindowState
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }


data BrowserPermissionType = BrowserPermissionTypeAccessibilityEvents | BrowserPermissionTypeAudioCapture | BrowserPermissionTypeBackgroundSync | BrowserPermissionTypeBackgroundFetch | BrowserPermissionTypeClipboardReadWrite | BrowserPermissionTypeClipboardSanitizedWrite | BrowserPermissionTypeDisplayCapture | BrowserPermissionTypeDurableStorage | BrowserPermissionTypeFlash | BrowserPermissionTypeGeolocation | BrowserPermissionTypeMidi | BrowserPermissionTypeMidiSysex | BrowserPermissionTypeNfc | BrowserPermissionTypeNotifications | BrowserPermissionTypePaymentHandler | BrowserPermissionTypePeriodicBackgroundSync | BrowserPermissionTypeProtectedMediaIdentifier | BrowserPermissionTypeSensors | BrowserPermissionTypeVideoCapture | BrowserPermissionTypeVideoCapturePanTiltZoom | BrowserPermissionTypeIdleDetection | BrowserPermissionTypeWakeLockScreen | BrowserPermissionTypeWakeLockSystem
   deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserPermissionType where
   parseJSON = A.withText  "BrowserPermissionType"  $ \v -> do
      case v of
         "accessibilityEvents" -> pure BrowserPermissionTypeAccessibilityEvents
         "audioCapture" -> pure BrowserPermissionTypeAudioCapture
         "backgroundSync" -> pure BrowserPermissionTypeBackgroundSync
         "backgroundFetch" -> pure BrowserPermissionTypeBackgroundFetch
         "clipboardReadWrite" -> pure BrowserPermissionTypeClipboardReadWrite
         "clipboardSanitizedWrite" -> pure BrowserPermissionTypeClipboardSanitizedWrite
         "displayCapture" -> pure BrowserPermissionTypeDisplayCapture
         "durableStorage" -> pure BrowserPermissionTypeDurableStorage
         "flash" -> pure BrowserPermissionTypeFlash
         "geolocation" -> pure BrowserPermissionTypeGeolocation
         "midi" -> pure BrowserPermissionTypeMidi
         "midiSysex" -> pure BrowserPermissionTypeMidiSysex
         "nfc" -> pure BrowserPermissionTypeNfc
         "notifications" -> pure BrowserPermissionTypeNotifications
         "paymentHandler" -> pure BrowserPermissionTypePaymentHandler
         "periodicBackgroundSync" -> pure BrowserPermissionTypePeriodicBackgroundSync
         "protectedMediaIdentifier" -> pure BrowserPermissionTypeProtectedMediaIdentifier
         "sensors" -> pure BrowserPermissionTypeSensors
         "videoCapture" -> pure BrowserPermissionTypeVideoCapture
         "videoCapturePanTiltZoom" -> pure BrowserPermissionTypeVideoCapturePanTiltZoom
         "idleDetection" -> pure BrowserPermissionTypeIdleDetection
         "wakeLockScreen" -> pure BrowserPermissionTypeWakeLockScreen
         "wakeLockSystem" -> pure BrowserPermissionTypeWakeLockSystem
         _ -> fail "failed to parse BrowserPermissionType"

instance ToJSON BrowserPermissionType where
   toJSON v = A.String $
      case v of
         BrowserPermissionTypeAccessibilityEvents -> "accessibilityEvents"
         BrowserPermissionTypeAudioCapture -> "audioCapture"
         BrowserPermissionTypeBackgroundSync -> "backgroundSync"
         BrowserPermissionTypeBackgroundFetch -> "backgroundFetch"
         BrowserPermissionTypeClipboardReadWrite -> "clipboardReadWrite"
         BrowserPermissionTypeClipboardSanitizedWrite -> "clipboardSanitizedWrite"
         BrowserPermissionTypeDisplayCapture -> "displayCapture"
         BrowserPermissionTypeDurableStorage -> "durableStorage"
         BrowserPermissionTypeFlash -> "flash"
         BrowserPermissionTypeGeolocation -> "geolocation"
         BrowserPermissionTypeMidi -> "midi"
         BrowserPermissionTypeMidiSysex -> "midiSysex"
         BrowserPermissionTypeNfc -> "nfc"
         BrowserPermissionTypeNotifications -> "notifications"
         BrowserPermissionTypePaymentHandler -> "paymentHandler"
         BrowserPermissionTypePeriodicBackgroundSync -> "periodicBackgroundSync"
         BrowserPermissionTypeProtectedMediaIdentifier -> "protectedMediaIdentifier"
         BrowserPermissionTypeSensors -> "sensors"
         BrowserPermissionTypeVideoCapture -> "videoCapture"
         BrowserPermissionTypeVideoCapturePanTiltZoom -> "videoCapturePanTiltZoom"
         BrowserPermissionTypeIdleDetection -> "idleDetection"
         BrowserPermissionTypeWakeLockScreen -> "wakeLockScreen"
         BrowserPermissionTypeWakeLockSystem -> "wakeLockSystem"


data BrowserPermissionSetting = BrowserPermissionSettingGranted | BrowserPermissionSettingDenied | BrowserPermissionSettingPrompt
   deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserPermissionSetting where
   parseJSON = A.withText  "BrowserPermissionSetting"  $ \v -> do
      case v of
         "granted" -> pure BrowserPermissionSettingGranted
         "denied" -> pure BrowserPermissionSettingDenied
         "prompt" -> pure BrowserPermissionSettingPrompt
         _ -> fail "failed to parse BrowserPermissionSetting"

instance ToJSON BrowserPermissionSetting where
   toJSON v = A.String $
      case v of
         BrowserPermissionSettingGranted -> "granted"
         BrowserPermissionSettingDenied -> "denied"
         BrowserPermissionSettingPrompt -> "prompt"



data BrowserPermissionDescriptor = BrowserPermissionDescriptor {
   browserPermissionDescriptorName :: String,
   browserPermissionDescriptorSysex :: Maybe Bool,
   browserPermissionDescriptorUserVisibleOnly :: Maybe Bool,
   browserPermissionDescriptorAllowWithoutSanitization :: Maybe Bool,
   browserPermissionDescriptorPanTiltZoom :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserPermissionDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  BrowserPermissionDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


data BrowserBrowserCommandId = BrowserBrowserCommandIdOpenTabSearch | BrowserBrowserCommandIdCloseTabSearch
   deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserBrowserCommandId where
   parseJSON = A.withText  "BrowserBrowserCommandId"  $ \v -> do
      case v of
         "openTabSearch" -> pure BrowserBrowserCommandIdOpenTabSearch
         "closeTabSearch" -> pure BrowserBrowserCommandIdCloseTabSearch
         _ -> fail "failed to parse BrowserBrowserCommandId"

instance ToJSON BrowserBrowserCommandId where
   toJSON v = A.String $
      case v of
         BrowserBrowserCommandIdOpenTabSearch -> "openTabSearch"
         BrowserBrowserCommandIdCloseTabSearch -> "closeTabSearch"



data BrowserBucket = BrowserBucket {
   browserBucketLow :: Int,
   browserBucketHigh :: Int,
   browserBucketCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBucket  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBucket where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



data BrowserHistogram = BrowserHistogram {
   browserHistogramName :: String,
   browserHistogramSum :: Int,
   browserHistogramCount :: Int,
   browserHistogramBuckets :: [BrowserBucket]
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  BrowserHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





data BrowserDownloadWillBegin = BrowserDownloadWillBegin {
   browserDownloadWillBeginFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
   browserDownloadWillBeginGuid :: String,
   browserDownloadWillBeginUrl :: String,
   browserDownloadWillBeginSuggestedFilename :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadWillBegin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadWillBegin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


data BrowserDownloadProgressState = BrowserDownloadProgressStateInProgress | BrowserDownloadProgressStateCompleted | BrowserDownloadProgressStateCanceled
   deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserDownloadProgressState where
   parseJSON = A.withText  "BrowserDownloadProgressState"  $ \v -> do
      case v of
         "inProgress" -> pure BrowserDownloadProgressStateInProgress
         "completed" -> pure BrowserDownloadProgressStateCompleted
         "canceled" -> pure BrowserDownloadProgressStateCanceled
         _ -> fail "failed to parse BrowserDownloadProgressState"

instance ToJSON BrowserDownloadProgressState where
   toJSON v = A.String $
      case v of
         BrowserDownloadProgressStateInProgress -> "inProgress"
         BrowserDownloadProgressStateCompleted -> "completed"
         BrowserDownloadProgressStateCanceled -> "canceled"



data BrowserDownloadProgress = BrowserDownloadProgress {
   browserDownloadProgressGuid :: String,
   browserDownloadProgressTotalBytes :: Double,
   browserDownloadProgressReceivedBytes :: Double,
   browserDownloadProgressState :: BrowserDownloadProgressState
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadProgress  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadProgress where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





data PBrowserSetPermission = PBrowserSetPermission {
   pBrowserSetPermissionPermission :: BrowserPermissionDescriptor,
   pBrowserSetPermissionSetting :: BrowserPermissionSetting,
   pBrowserSetPermissionOrigin :: Maybe String,
   pBrowserSetPermissionBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetPermission  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetPermission where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


browserSetPermission :: Handle ev -> PBrowserSetPermission -> IO (Maybe Error)
browserSetPermission handle params = sendReceiveCommand handle "Browser.setPermission" (Just params)



data PBrowserGrantPermissions = PBrowserGrantPermissions {
   pBrowserGrantPermissionsPermissions :: [BrowserPermissionType],
   pBrowserGrantPermissionsOrigin :: Maybe String,
   pBrowserGrantPermissionsBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGrantPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserGrantPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


browserGrantPermissions :: Handle ev -> PBrowserGrantPermissions -> IO (Maybe Error)
browserGrantPermissions handle params = sendReceiveCommand handle "Browser.grantPermissions" (Just params)



data PBrowserResetPermissions = PBrowserResetPermissions {
   pBrowserResetPermissionsBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserResetPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserResetPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


browserResetPermissions :: Handle ev -> PBrowserResetPermissions -> IO (Maybe Error)
browserResetPermissions handle params = sendReceiveCommand handle "Browser.resetPermissions" (Just params)


data PBrowserSetDownloadBehaviorBehavior = PBrowserSetDownloadBehaviorBehaviorDeny | PBrowserSetDownloadBehaviorBehaviorAllow | PBrowserSetDownloadBehaviorBehaviorAllowAndName | PBrowserSetDownloadBehaviorBehaviorDefault
   deriving (Ord, Eq, Show, Read)
instance FromJSON PBrowserSetDownloadBehaviorBehavior where
   parseJSON = A.withText  "PBrowserSetDownloadBehaviorBehavior"  $ \v -> do
      case v of
         "deny" -> pure PBrowserSetDownloadBehaviorBehaviorDeny
         "allow" -> pure PBrowserSetDownloadBehaviorBehaviorAllow
         "allowAndName" -> pure PBrowserSetDownloadBehaviorBehaviorAllowAndName
         "default" -> pure PBrowserSetDownloadBehaviorBehaviorDefault
         _ -> fail "failed to parse PBrowserSetDownloadBehaviorBehavior"

instance ToJSON PBrowserSetDownloadBehaviorBehavior where
   toJSON v = A.String $
      case v of
         PBrowserSetDownloadBehaviorBehaviorDeny -> "deny"
         PBrowserSetDownloadBehaviorBehaviorAllow -> "allow"
         PBrowserSetDownloadBehaviorBehaviorAllowAndName -> "allowAndName"
         PBrowserSetDownloadBehaviorBehaviorDefault -> "default"



data PBrowserSetDownloadBehavior = PBrowserSetDownloadBehavior {
   pBrowserSetDownloadBehaviorBehavior :: PBrowserSetDownloadBehaviorBehavior,
   pBrowserSetDownloadBehaviorBrowserContextId :: Maybe BrowserBrowserContextId,
   pBrowserSetDownloadBehaviorDownloadPath :: Maybe String,
   pBrowserSetDownloadBehaviorEventsEnabled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDownloadBehavior  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDownloadBehavior where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


browserSetDownloadBehavior :: Handle ev -> PBrowserSetDownloadBehavior -> IO (Maybe Error)
browserSetDownloadBehavior handle params = sendReceiveCommand handle "Browser.setDownloadBehavior" (Just params)



data PBrowserCancelDownload = PBrowserCancelDownload {
   pBrowserCancelDownloadGuid :: String,
   pBrowserCancelDownloadBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserCancelDownload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PBrowserCancelDownload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


browserCancelDownload :: Handle ev -> PBrowserCancelDownload -> IO (Maybe Error)
browserCancelDownload handle params = sendReceiveCommand handle "Browser.cancelDownload" (Just params)


browserClose :: Handle ev -> IO (Maybe Error)
browserClose handle = sendReceiveCommand handle "Browser.close" (Nothing :: Maybe ())


browserCrash :: Handle ev -> IO (Maybe Error)
browserCrash handle = sendReceiveCommand handle "Browser.crash" (Nothing :: Maybe ())


browserCrashGpuProcess :: Handle ev -> IO (Maybe Error)
browserCrashGpuProcess handle = sendReceiveCommand handle "Browser.crashGpuProcess" (Nothing :: Maybe ())


browserGetVersion :: Handle ev -> IO (Either Error BrowserGetVersion)
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())

data BrowserGetVersion = BrowserGetVersion {
   browserGetVersionProtocolVersion :: String,
   browserGetVersionProduct :: String,
   browserGetVersionRevision :: String,
   browserGetVersionUserAgent :: String,
   browserGetVersionJsVersion :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command BrowserGetVersion where
   commandName _ = "Browser.getVersion"



browserGetBrowserCommandLine :: Handle ev -> IO (Either Error BrowserGetBrowserCommandLine)
browserGetBrowserCommandLine handle = sendReceiveCommandResult handle "Browser.getBrowserCommandLine" (Nothing :: Maybe ())

data BrowserGetBrowserCommandLine = BrowserGetBrowserCommandLine {
   browserGetBrowserCommandLineArguments :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetBrowserCommandLine where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command BrowserGetBrowserCommandLine where
   commandName _ = "Browser.getBrowserCommandLine"




data PBrowserGetHistograms = PBrowserGetHistograms {
   pBrowserGetHistogramsQuery :: Maybe String,
   pBrowserGetHistogramsDelta :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistograms  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


browserGetHistograms :: Handle ev -> PBrowserGetHistograms -> IO (Either Error BrowserGetHistograms)
browserGetHistograms handle params = sendReceiveCommandResult handle "Browser.getHistograms" (Just params)

data BrowserGetHistograms = BrowserGetHistograms {
   browserGetHistogramsHistograms :: [BrowserHistogram]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command BrowserGetHistograms where
   commandName _ = "Browser.getHistograms"




data PBrowserGetHistogram = PBrowserGetHistogram {
   pBrowserGetHistogramName :: String,
   pBrowserGetHistogramDelta :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


browserGetHistogram :: Handle ev -> PBrowserGetHistogram -> IO (Either Error BrowserGetHistogram)
browserGetHistogram handle params = sendReceiveCommandResult handle "Browser.getHistogram" (Just params)

data BrowserGetHistogram = BrowserGetHistogram {
   browserGetHistogramHistogram :: BrowserHistogram
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command BrowserGetHistogram where
   commandName _ = "Browser.getHistogram"




data PBrowserGetWindowBounds = PBrowserGetWindowBounds {
   pBrowserGetWindowBoundsWindowId :: BrowserWindowId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


browserGetWindowBounds :: Handle ev -> PBrowserGetWindowBounds -> IO (Either Error BrowserGetWindowBounds)
browserGetWindowBounds handle params = sendReceiveCommandResult handle "Browser.getWindowBounds" (Just params)

data BrowserGetWindowBounds = BrowserGetWindowBounds {
   browserGetWindowBoundsBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command BrowserGetWindowBounds where
   commandName _ = "Browser.getWindowBounds"




data PBrowserGetWindowForTarget = PBrowserGetWindowForTarget {
   pBrowserGetWindowForTargetTargetId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowForTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


browserGetWindowForTarget :: Handle ev -> PBrowserGetWindowForTarget -> IO (Either Error BrowserGetWindowForTarget)
browserGetWindowForTarget handle params = sendReceiveCommandResult handle "Browser.getWindowForTarget" (Just params)

data BrowserGetWindowForTarget = BrowserGetWindowForTarget {
   browserGetWindowForTargetWindowId :: BrowserWindowId,
   browserGetWindowForTargetBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command BrowserGetWindowForTarget where
   commandName _ = "Browser.getWindowForTarget"




data PBrowserSetWindowBounds = PBrowserSetWindowBounds {
   pBrowserSetWindowBoundsWindowId :: BrowserWindowId,
   pBrowserSetWindowBoundsBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


browserSetWindowBounds :: Handle ev -> PBrowserSetWindowBounds -> IO (Maybe Error)
browserSetWindowBounds handle params = sendReceiveCommand handle "Browser.setWindowBounds" (Just params)



data PBrowserSetDockTile = PBrowserSetDockTile {
   pBrowserSetDockTileBadgeLabel :: Maybe String,
   pBrowserSetDockTileImage :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDockTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDockTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


browserSetDockTile :: Handle ev -> PBrowserSetDockTile -> IO (Maybe Error)
browserSetDockTile handle params = sendReceiveCommand handle "Browser.setDockTile" (Just params)



data PBrowserExecuteBrowserCommand = PBrowserExecuteBrowserCommand {
   pBrowserExecuteBrowserCommandCommandId :: BrowserBrowserCommandId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserExecuteBrowserCommand  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PBrowserExecuteBrowserCommand where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


browserExecuteBrowserCommand :: Handle ev -> PBrowserExecuteBrowserCommand -> IO (Maybe Error)
browserExecuteBrowserCommand handle params = sendReceiveCommand handle "Browser.executeBrowserCommand" (Just params)



type TargetTargetId = String
type TargetSessionId = String

data TargetTargetInfo = TargetTargetInfo {
   targetTargetInfoTargetId :: TargetTargetId,
   targetTargetInfoType :: String,
   targetTargetInfoTitle :: String,
   targetTargetInfoUrl :: String,
   targetTargetInfoAttached :: Bool,
   targetTargetInfoOpenerId :: Maybe TargetTargetId,
   targetTargetInfoCanAccessOpener :: Bool,
   targetTargetInfoOpenerFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
   targetTargetInfoBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



data TargetRemoteLocation = TargetRemoteLocation {
   targetRemoteLocationHost :: String,
   targetRemoteLocationPort :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetRemoteLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  TargetRemoteLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





data TargetAttachedToTarget = TargetAttachedToTarget {
   targetAttachedToTargetSessionId :: TargetSessionId,
   targetAttachedToTargetTargetInfo :: TargetTargetInfo,
   targetAttachedToTargetWaitingForDebugger :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetAttachedToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  TargetAttachedToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



data TargetDetachedFromTarget = TargetDetachedFromTarget {
   targetDetachedFromTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetDetachedFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  TargetDetachedFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



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



targetAttachToBrowserTarget :: Handle ev -> IO (Either Error TargetAttachToBrowserTarget)
targetAttachToBrowserTarget handle = sendReceiveCommandResult handle "Target.attachToBrowserTarget" (Nothing :: Maybe ())

data TargetAttachToBrowserTarget = TargetAttachToBrowserTarget {
   targetAttachToBrowserTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToBrowserTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command TargetAttachToBrowserTarget where
   commandName _ = "Target.attachToBrowserTarget"




data PTargetCloseTarget = PTargetCloseTarget {
   pTargetCloseTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCloseTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PTargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


targetCloseTarget :: Handle ev -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget handle params = sendReceiveCommand handle "Target.closeTarget" (Just params)



data PTargetExposeDevToolsProtocol = PTargetExposeDevToolsProtocol {
   pTargetExposeDevToolsProtocolTargetId :: TargetTargetId,
   pTargetExposeDevToolsProtocolBindingName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetExposeDevToolsProtocol  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PTargetExposeDevToolsProtocol where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


targetExposeDevToolsProtocol :: Handle ev -> PTargetExposeDevToolsProtocol -> IO (Maybe Error)
targetExposeDevToolsProtocol handle params = sendReceiveCommand handle "Target.exposeDevToolsProtocol" (Just params)



data PTargetCreateBrowserContext = PTargetCreateBrowserContext {
   pTargetCreateBrowserContextDisposeOnDetach :: Maybe Bool,
   pTargetCreateBrowserContextProxyServer :: Maybe String,
   pTargetCreateBrowserContextProxyBypassList :: Maybe String,
   pTargetCreateBrowserContextOriginsWithUniversalNetworkAccess :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


targetCreateBrowserContext :: Handle ev -> PTargetCreateBrowserContext -> IO (Either Error TargetCreateBrowserContext)
targetCreateBrowserContext handle params = sendReceiveCommandResult handle "Target.createBrowserContext" (Just params)

data TargetCreateBrowserContext = TargetCreateBrowserContext {
   targetCreateBrowserContextBrowserContextId :: BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command TargetCreateBrowserContext where
   commandName _ = "Target.createBrowserContext"



targetGetBrowserContexts :: Handle ev -> IO (Either Error TargetGetBrowserContexts)
targetGetBrowserContexts handle = sendReceiveCommandResult handle "Target.getBrowserContexts" (Nothing :: Maybe ())

data TargetGetBrowserContexts = TargetGetBrowserContexts {
   targetGetBrowserContextsBrowserContextIds :: [BrowserBrowserContextId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetBrowserContexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command TargetGetBrowserContexts where
   commandName _ = "Target.getBrowserContexts"




data PTargetCreateTarget = PTargetCreateTarget {
   pTargetCreateTargetUrl :: String,
   pTargetCreateTargetWidth :: Maybe Int,
   pTargetCreateTargetHeight :: Maybe Int,
   pTargetCreateTargetBrowserContextId :: Maybe BrowserBrowserContextId,
   pTargetCreateTargetEnableBeginFrameControl :: Maybe Bool,
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



data PTargetDisposeBrowserContext = PTargetDisposeBrowserContext {
   pTargetDisposeBrowserContextBrowserContextId :: BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDisposeBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PTargetDisposeBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


targetDisposeBrowserContext :: Handle ev -> PTargetDisposeBrowserContext -> IO (Maybe Error)
targetDisposeBrowserContext handle params = sendReceiveCommand handle "Target.disposeBrowserContext" (Just params)



data PTargetGetTargetInfo = PTargetGetTargetInfo {
   pTargetGetTargetInfoTargetId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetGetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


targetGetTargetInfo :: Handle ev -> PTargetGetTargetInfo -> IO (Either Error TargetGetTargetInfo)
targetGetTargetInfo handle params = sendReceiveCommandResult handle "Target.getTargetInfo" (Just params)

data TargetGetTargetInfo = TargetGetTargetInfo {
   targetGetTargetInfoTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command TargetGetTargetInfo where
   commandName _ = "Target.getTargetInfo"



targetGetTargets :: Handle ev -> IO (Either Error TargetGetTargets)
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())

data TargetGetTargets = TargetGetTargets {
   targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command TargetGetTargets where
   commandName _ = "Target.getTargets"




data PTargetSetAutoAttach = PTargetSetAutoAttach {
   pTargetSetAutoAttachAutoAttach :: Bool,
   pTargetSetAutoAttachWaitForDebuggerOnStart :: Bool,
   pTargetSetAutoAttachFlatten :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetAutoAttach  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetSetAutoAttach where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


targetSetAutoAttach :: Handle ev -> PTargetSetAutoAttach -> IO (Maybe Error)
targetSetAutoAttach handle params = sendReceiveCommand handle "Target.setAutoAttach" (Just params)



data PTargetAutoAttachRelated = PTargetAutoAttachRelated {
   pTargetAutoAttachRelatedTargetId :: TargetTargetId,
   pTargetAutoAttachRelatedWaitForDebuggerOnStart :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAutoAttachRelated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PTargetAutoAttachRelated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


targetAutoAttachRelated :: Handle ev -> PTargetAutoAttachRelated -> IO (Maybe Error)
targetAutoAttachRelated handle params = sendReceiveCommand handle "Target.autoAttachRelated" (Just params)



data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
   pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetDiscoverTargets  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetDiscoverTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


targetSetDiscoverTargets :: Handle ev -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)



data PTargetSetRemoteLocations = PTargetSetRemoteLocations {
   pTargetSetRemoteLocationsLocations :: [TargetRemoteLocation]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetRemoteLocations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetRemoteLocations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


targetSetRemoteLocations :: Handle ev -> PTargetSetRemoteLocations -> IO (Maybe Error)
targetSetRemoteLocations handle params = sendReceiveCommand handle "Target.setRemoteLocations" (Just params)



