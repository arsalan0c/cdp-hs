{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
  Browser :
     The Browser domain defines methods and events for browser managing.

  Target :
     Supports additional targets discovery and allows to attach to them.

-}


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


-- | Type 'Browser.BrowserContextID'.
type BrowserBrowserContextId = String

-- | Type 'Browser.WindowID'.
type BrowserWindowId = Int

-- | Type 'Browser.WindowState'.
--   The state of the browser window.
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



-- | Type 'Browser.Bounds'.
--   Browser window bounds information
data BrowserBounds = BrowserBounds {
  -- | The offset from the left edge of the screen to the window in pixels.
  browserBoundsLeft :: Maybe Int,
  -- | The offset from the top edge of the screen to the window in pixels.
  browserBoundsTop :: Maybe Int,
  -- | The window width in pixels.
  browserBoundsWidth :: Maybe Int,
  -- | The window height in pixels.
  browserBoundsHeight :: Maybe Int,
  -- | The window state. Default to normal.
  browserBoundsWindowState :: Maybe BrowserWindowState
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Browser.PermissionType'.
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



-- | Type 'Browser.PermissionSetting'.
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



-- | Type 'Browser.PermissionDescriptor'.
--   Definition of PermissionDescriptor defined in the Permissions API:
--   https://w3c.github.io/permissions/#dictdef-permissiondescriptor.
data BrowserPermissionDescriptor = BrowserPermissionDescriptor {
  -- | Name of permission.
  --   See https://cs.chromium.org/chromium/src/third_party/blink/renderer/modules/permissions/permission_descriptor.idl for valid permission names.
  browserPermissionDescriptorName :: String,
  -- | For "midi" permission, may also specify sysex control.
  browserPermissionDescriptorSysex :: Maybe Bool,
  -- | For "push" permission, may specify userVisibleOnly.
  --   Note that userVisibleOnly = true is the only currently supported type.
  browserPermissionDescriptorUserVisibleOnly :: Maybe Bool,
  -- | For "clipboard" permission, may specify allowWithoutSanitization.
  browserPermissionDescriptorAllowWithoutSanitization :: Maybe Bool,
  -- | For "camera" permission, may specify panTiltZoom.
  browserPermissionDescriptorPanTiltZoom :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserPermissionDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  BrowserPermissionDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Type 'Browser.BrowserCommandId'.
--   Browser command ids used by executeBrowserCommand.
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



-- | Type 'Browser.Bucket'.
--   Chrome histogram bucket.
data BrowserBucket = BrowserBucket {
  -- | Minimum value (inclusive).
  browserBucketLow :: Int,
  -- | Maximum value (exclusive).
  browserBucketHigh :: Int,
  -- | Number of samples.
  browserBucketCount :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBucket  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBucket where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Browser.Histogram'.
--   Chrome histogram.
data BrowserHistogram = BrowserHistogram {
  -- | Name.
  browserHistogramName :: String,
  -- | Sum of sample values.
  browserHistogramSum :: Int,
  -- | Total number of samples.
  browserHistogramCount :: Int,
  -- | Buckets.
  browserHistogramBuckets :: [BrowserBucket]
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  BrowserHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





-- | Type of the 'Browser.downloadWillBegin' event.
data BrowserDownloadWillBegin = BrowserDownloadWillBegin {
  -- | Id of the frame that caused the download to begin.
  browserDownloadWillBeginFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
  -- | Global unique identifier of the download.
  browserDownloadWillBeginGuid :: String,
  -- | URL of the resource being downloaded.
  browserDownloadWillBeginUrl :: String,
  -- | Suggested file name of the resource (the actual name of the file saved on disk may differ).
  browserDownloadWillBeginSuggestedFilename :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadWillBegin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadWillBegin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event BrowserDownloadWillBegin where
    eventName _ = "Browser.downloadWillBegin"

-- | Type of the 'Browser.downloadProgress' event.
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
  -- | Global unique identifier of the download.
  browserDownloadProgressGuid :: String,
  -- | Total expected bytes to download.
  browserDownloadProgressTotalBytes :: Double,
  -- | Total bytes received.
  browserDownloadProgressReceivedBytes :: Double,
  -- | Download status.
  browserDownloadProgressState :: BrowserDownloadProgressState
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadProgress  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadProgress where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event BrowserDownloadProgress where
    eventName _ = "Browser.downloadProgress"



-- | Parameters of the 'browserSetPermission' command.
data PBrowserSetPermission = PBrowserSetPermission {
  -- | Descriptor of permission to override.
  pBrowserSetPermissionPermission :: BrowserPermissionDescriptor,
  -- | Setting of the permission.
  pBrowserSetPermissionSetting :: BrowserPermissionSetting,
  -- | Origin the permission applies to, all origins if not specified.
  pBrowserSetPermissionOrigin :: Maybe String,
  -- | Context to override. When omitted, default browser context is used.
  pBrowserSetPermissionBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetPermission  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetPermission where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Browser.setPermission' command.
--   Set permission settings for given origin.
--   Parameters: 'PBrowserSetPermission'
browserSetPermission :: Handle ev -> PBrowserSetPermission -> IO ()
browserSetPermission handle params = sendReceiveCommand handle "Browser.setPermission" (Just params)


-- | Parameters of the 'browserGrantPermissions' command.
data PBrowserGrantPermissions = PBrowserGrantPermissions {
  pBrowserGrantPermissionsPermissions :: [BrowserPermissionType],
  -- | Origin the permission applies to, all origins if not specified.
  pBrowserGrantPermissionsOrigin :: Maybe String,
  -- | BrowserContext to override permissions. When omitted, default browser context is used.
  pBrowserGrantPermissionsBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGrantPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserGrantPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Browser.grantPermissions' command.
--   Grant specific permissions to the given origin and reject all others.
--   Parameters: 'PBrowserGrantPermissions'
browserGrantPermissions :: Handle ev -> PBrowserGrantPermissions -> IO ()
browserGrantPermissions handle params = sendReceiveCommand handle "Browser.grantPermissions" (Just params)


-- | Parameters of the 'browserResetPermissions' command.
data PBrowserResetPermissions = PBrowserResetPermissions {
  -- | BrowserContext to reset permissions. When omitted, default browser context is used.
  pBrowserResetPermissionsBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserResetPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserResetPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Browser.resetPermissions' command.
--   Reset all permission management for all origins.
--   Parameters: 'PBrowserResetPermissions'
browserResetPermissions :: Handle ev -> PBrowserResetPermissions -> IO ()
browserResetPermissions handle params = sendReceiveCommand handle "Browser.resetPermissions" (Just params)


-- | Parameters of the 'browserSetDownloadBehavior' command.
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
  -- | Whether to allow all or deny all download requests, or use default Chrome behavior if
  --   available (otherwise deny). |allowAndName| allows download and names files according to
  --   their dowmload guids.
  pBrowserSetDownloadBehaviorBehavior :: PBrowserSetDownloadBehaviorBehavior,
  -- | BrowserContext to set download behavior. When omitted, default browser context is used.
  pBrowserSetDownloadBehaviorBrowserContextId :: Maybe BrowserBrowserContextId,
  -- | The default path to save downloaded files to. This is required if behavior is set to 'allow'
  --   or 'allowAndName'.
  pBrowserSetDownloadBehaviorDownloadPath :: Maybe String,
  -- | Whether to emit download events (defaults to false).
  pBrowserSetDownloadBehaviorEventsEnabled :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDownloadBehavior  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDownloadBehavior where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Browser.setDownloadBehavior' command.
--   Set the behavior when downloading a file.
--   Parameters: 'PBrowserSetDownloadBehavior'
browserSetDownloadBehavior :: Handle ev -> PBrowserSetDownloadBehavior -> IO ()
browserSetDownloadBehavior handle params = sendReceiveCommand handle "Browser.setDownloadBehavior" (Just params)


-- | Parameters of the 'browserCancelDownload' command.
data PBrowserCancelDownload = PBrowserCancelDownload {
  -- | Global unique identifier of the download.
  pBrowserCancelDownloadGuid :: String,
  -- | BrowserContext to perform the action in. When omitted, default browser context is used.
  pBrowserCancelDownloadBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserCancelDownload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PBrowserCancelDownload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the 'Browser.cancelDownload' command.
--   Cancel a download if in progress
--   Parameters: 'PBrowserCancelDownload'
browserCancelDownload :: Handle ev -> PBrowserCancelDownload -> IO ()
browserCancelDownload handle params = sendReceiveCommand handle "Browser.cancelDownload" (Just params)


-- | Function for the 'Browser.close' command.
--   Close browser gracefully.
browserClose :: Handle ev -> IO ()
browserClose handle = sendReceiveCommand handle "Browser.close" (Nothing :: Maybe ())


-- | Function for the 'Browser.crash' command.
--   Crashes browser on the main thread.
browserCrash :: Handle ev -> IO ()
browserCrash handle = sendReceiveCommand handle "Browser.crash" (Nothing :: Maybe ())


-- | Function for the 'Browser.crashGpuProcess' command.
--   Crashes GPU process.
browserCrashGpuProcess :: Handle ev -> IO ()
browserCrashGpuProcess handle = sendReceiveCommand handle "Browser.crashGpuProcess" (Nothing :: Maybe ())


-- | Function for the 'Browser.getVersion' command.
--   Returns version information.
--   Returns: 'BrowserGetVersion'
browserGetVersion :: Handle ev -> IO BrowserGetVersion
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())

-- | Return type of the 'browserGetVersion' command.
data BrowserGetVersion = BrowserGetVersion {
  -- | Protocol version.
  browserGetVersionProtocolVersion :: String,
  -- | Product name.
  browserGetVersionProduct :: String,
  -- | Product revision.
  browserGetVersionRevision :: String,
  -- | User-Agent.
  browserGetVersionUserAgent :: String,
  -- | V8 version.
  browserGetVersionJsVersion :: String
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command BrowserGetVersion where
   commandName _ = "Browser.getVersion"



-- | Function for the 'Browser.getBrowserCommandLine' command.
--   Returns the command line switches for the browser process if, and only if
--   --enable-automation is on the commandline.
--   Returns: 'BrowserGetBrowserCommandLine'
browserGetBrowserCommandLine :: Handle ev -> IO BrowserGetBrowserCommandLine
browserGetBrowserCommandLine handle = sendReceiveCommandResult handle "Browser.getBrowserCommandLine" (Nothing :: Maybe ())

-- | Return type of the 'browserGetBrowserCommandLine' command.
data BrowserGetBrowserCommandLine = BrowserGetBrowserCommandLine {
  -- | Commandline parameters
  browserGetBrowserCommandLineArguments :: [String]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetBrowserCommandLine where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command BrowserGetBrowserCommandLine where
   commandName _ = "Browser.getBrowserCommandLine"



-- | Parameters of the 'browserGetHistograms' command.
data PBrowserGetHistograms = PBrowserGetHistograms {
  -- | Requested substring in name. Only histograms which have query as a
  --   substring in their name are extracted. An empty or absent query returns
  --   all histograms.
  pBrowserGetHistogramsQuery :: Maybe String,
  -- | If true, retrieve delta since last call.
  pBrowserGetHistogramsDelta :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistograms  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Browser.getHistograms' command.
--   Get Chrome histograms.
--   Parameters: 'PBrowserGetHistograms'
--   Returns: 'BrowserGetHistograms'
browserGetHistograms :: Handle ev -> PBrowserGetHistograms -> IO BrowserGetHistograms
browserGetHistograms handle params = sendReceiveCommandResult handle "Browser.getHistograms" (Just params)

-- | Return type of the 'browserGetHistograms' command.
data BrowserGetHistograms = BrowserGetHistograms {
  -- | Histograms.
  browserGetHistogramsHistograms :: [BrowserHistogram]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command BrowserGetHistograms where
   commandName _ = "Browser.getHistograms"



-- | Parameters of the 'browserGetHistogram' command.
data PBrowserGetHistogram = PBrowserGetHistogram {
  -- | Requested histogram name.
  pBrowserGetHistogramName :: String,
  -- | If true, retrieve delta since last call.
  pBrowserGetHistogramDelta :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Browser.getHistogram' command.
--   Get a Chrome histogram by name.
--   Parameters: 'PBrowserGetHistogram'
--   Returns: 'BrowserGetHistogram'
browserGetHistogram :: Handle ev -> PBrowserGetHistogram -> IO BrowserGetHistogram
browserGetHistogram handle params = sendReceiveCommandResult handle "Browser.getHistogram" (Just params)

-- | Return type of the 'browserGetHistogram' command.
data BrowserGetHistogram = BrowserGetHistogram {
  -- | Histogram.
  browserGetHistogramHistogram :: BrowserHistogram
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command BrowserGetHistogram where
   commandName _ = "Browser.getHistogram"



-- | Parameters of the 'browserGetWindowBounds' command.
data PBrowserGetWindowBounds = PBrowserGetWindowBounds {
  -- | Browser window id.
  pBrowserGetWindowBoundsWindowId :: BrowserWindowId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Browser.getWindowBounds' command.
--   Get position and size of the browser window.
--   Parameters: 'PBrowserGetWindowBounds'
--   Returns: 'BrowserGetWindowBounds'
browserGetWindowBounds :: Handle ev -> PBrowserGetWindowBounds -> IO BrowserGetWindowBounds
browserGetWindowBounds handle params = sendReceiveCommandResult handle "Browser.getWindowBounds" (Just params)

-- | Return type of the 'browserGetWindowBounds' command.
data BrowserGetWindowBounds = BrowserGetWindowBounds {
  -- | Bounds information of the window. When window state is 'minimized', the restored window
  --   position and size are returned.
  browserGetWindowBoundsBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command BrowserGetWindowBounds where
   commandName _ = "Browser.getWindowBounds"



-- | Parameters of the 'browserGetWindowForTarget' command.
data PBrowserGetWindowForTarget = PBrowserGetWindowForTarget {
  -- | Devtools agent host id. If called as a part of the session, associated targetId is used.
  pBrowserGetWindowForTargetTargetId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowForTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the 'Browser.getWindowForTarget' command.
--   Get the browser window that contains the devtools target.
--   Parameters: 'PBrowserGetWindowForTarget'
--   Returns: 'BrowserGetWindowForTarget'
browserGetWindowForTarget :: Handle ev -> PBrowserGetWindowForTarget -> IO BrowserGetWindowForTarget
browserGetWindowForTarget handle params = sendReceiveCommandResult handle "Browser.getWindowForTarget" (Just params)

-- | Return type of the 'browserGetWindowForTarget' command.
data BrowserGetWindowForTarget = BrowserGetWindowForTarget {
  -- | Browser window id.
  browserGetWindowForTargetWindowId :: BrowserWindowId,
  -- | Bounds information of the window. When window state is 'minimized', the restored window
  --   position and size are returned.
  browserGetWindowForTargetBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command BrowserGetWindowForTarget where
   commandName _ = "Browser.getWindowForTarget"



-- | Parameters of the 'browserSetWindowBounds' command.
data PBrowserSetWindowBounds = PBrowserSetWindowBounds {
  -- | Browser window id.
  pBrowserSetWindowBoundsWindowId :: BrowserWindowId,
  -- | New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined
  --   with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.
  pBrowserSetWindowBoundsBounds :: BrowserBounds
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Browser.setWindowBounds' command.
--   Set position and/or size of the browser window.
--   Parameters: 'PBrowserSetWindowBounds'
browserSetWindowBounds :: Handle ev -> PBrowserSetWindowBounds -> IO ()
browserSetWindowBounds handle params = sendReceiveCommand handle "Browser.setWindowBounds" (Just params)


-- | Parameters of the 'browserSetDockTile' command.
data PBrowserSetDockTile = PBrowserSetDockTile {
  pBrowserSetDockTileBadgeLabel :: Maybe String,
  -- | Png encoded image. (Encoded as a base64 string when passed over JSON)
  pBrowserSetDockTileImage :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDockTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDockTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'Browser.setDockTile' command.
--   Set dock tile details, platform-specific.
--   Parameters: 'PBrowserSetDockTile'
browserSetDockTile :: Handle ev -> PBrowserSetDockTile -> IO ()
browserSetDockTile handle params = sendReceiveCommand handle "Browser.setDockTile" (Just params)


-- | Parameters of the 'browserExecuteBrowserCommand' command.
data PBrowserExecuteBrowserCommand = PBrowserExecuteBrowserCommand {
  pBrowserExecuteBrowserCommandCommandId :: BrowserBrowserCommandId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserExecuteBrowserCommand  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PBrowserExecuteBrowserCommand where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Browser.executeBrowserCommand' command.
--   Invoke custom browser commands used by telemetry.
--   Parameters: 'PBrowserExecuteBrowserCommand'
browserExecuteBrowserCommand :: Handle ev -> PBrowserExecuteBrowserCommand -> IO ()
browserExecuteBrowserCommand handle params = sendReceiveCommand handle "Browser.executeBrowserCommand" (Just params)



-- | Type 'Target.TargetID'.
type TargetTargetId = String

-- | Type 'Target.SessionID'.
--   Unique identifier of attached debugging session.
type TargetSessionId = String

-- | Type 'Target.TargetInfo'.
data TargetTargetInfo = TargetTargetInfo {
  targetTargetInfoTargetId :: TargetTargetId,
  targetTargetInfoType :: String,
  targetTargetInfoTitle :: String,
  targetTargetInfoUrl :: String,
  -- | Whether the target has an attached client.
  targetTargetInfoAttached :: Bool,
  -- | Opener target Id
  targetTargetInfoOpenerId :: Maybe TargetTargetId,
  -- | Whether the target has access to the originating window.
  targetTargetInfoCanAccessOpener :: Bool,
  -- | Frame id of originating window (is only set if target has an opener).
  targetTargetInfoOpenerFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
  targetTargetInfoBrowserContextId :: Maybe BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Target.RemoteLocation'.
data TargetRemoteLocation = TargetRemoteLocation {
  targetRemoteLocationHost :: String,
  targetRemoteLocationPort :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetRemoteLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  TargetRemoteLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Type of the 'Target.attachedToTarget' event.
data TargetAttachedToTarget = TargetAttachedToTarget {
  -- | Identifier assigned to the session used to send/receive messages.
  targetAttachedToTargetSessionId :: TargetSessionId,
  targetAttachedToTargetTargetInfo :: TargetTargetInfo,
  targetAttachedToTargetWaitingForDebugger :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetAttachedToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  TargetAttachedToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Event TargetAttachedToTarget where
    eventName _ = "Target.attachedToTarget"

-- | Type of the 'Target.detachedFromTarget' event.
data TargetDetachedFromTarget = TargetDetachedFromTarget {
  -- | Detached session identifier.
  targetDetachedFromTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetDetachedFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  TargetDetachedFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Event TargetDetachedFromTarget where
    eventName _ = "Target.detachedFromTarget"

-- | Type of the 'Target.receivedMessageFromTarget' event.
data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
  -- | Identifier of a session which sends a message.
  targetReceivedMessageFromTargetSessionId :: TargetSessionId,
  targetReceivedMessageFromTargetMessage :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetReceivedMessageFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  TargetReceivedMessageFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }


instance Event TargetReceivedMessageFromTarget where
    eventName _ = "Target.receivedMessageFromTarget"

-- | Type of the 'Target.targetCreated' event.
data TargetTargetCreated = TargetTargetCreated {
  targetTargetCreatedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event TargetTargetCreated where
    eventName _ = "Target.targetCreated"

-- | Type of the 'Target.targetDestroyed' event.
data TargetTargetDestroyed = TargetTargetDestroyed {
  targetTargetDestroyedTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  TargetTargetDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


instance Event TargetTargetDestroyed where
    eventName _ = "Target.targetDestroyed"

-- | Type of the 'Target.targetCrashed' event.
data TargetTargetCrashed = TargetTargetCrashed {
  targetTargetCrashedTargetId :: TargetTargetId,
  -- | Termination status type.
  targetTargetCrashedStatus :: String,
  -- | Termination error code.
  targetTargetCrashedErrorCode :: Int
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCrashed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCrashed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


instance Event TargetTargetCrashed where
    eventName _ = "Target.targetCrashed"

-- | Type of the 'Target.targetInfoChanged' event.
data TargetTargetInfoChanged = TargetTargetInfoChanged {
  targetTargetInfoChangedTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfoChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfoChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


instance Event TargetTargetInfoChanged where
    eventName _ = "Target.targetInfoChanged"



-- | Parameters of the 'targetActivateTarget' command.
data PTargetActivateTarget = PTargetActivateTarget {
  pTargetActivateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetActivateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetActivateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Target.activateTarget' command.
--   Activates (focuses) the target.
--   Parameters: 'PTargetActivateTarget'
targetActivateTarget :: Handle ev -> PTargetActivateTarget -> IO ()
targetActivateTarget handle params = sendReceiveCommand handle "Target.activateTarget" (Just params)


-- | Parameters of the 'targetAttachToTarget' command.
data PTargetAttachToTarget = PTargetAttachToTarget {
  pTargetAttachToTargetTargetId :: TargetTargetId,
  -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
  --   We plan to make this the default, deprecate non-flattened mode,
  --   and eventually retire it. See crbug.com/991325.
  pTargetAttachToTargetFlatten :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAttachToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the 'Target.attachToTarget' command.
--   Attaches to the target with given id.
--   Parameters: 'PTargetAttachToTarget'
--   Returns: 'TargetAttachToTarget'
targetAttachToTarget :: Handle ev -> PTargetAttachToTarget -> IO TargetAttachToTarget
targetAttachToTarget handle params = sendReceiveCommandResult handle "Target.attachToTarget" (Just params)

-- | Return type of the 'targetAttachToTarget' command.
data TargetAttachToTarget = TargetAttachToTarget {
  -- | Id assigned to the session.
  targetAttachToTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TargetAttachToTarget where
   commandName _ = "Target.attachToTarget"



-- | Function for the 'Target.attachToBrowserTarget' command.
--   Attaches to the browser target, only uses flat sessionId mode.
--   Returns: 'TargetAttachToBrowserTarget'
targetAttachToBrowserTarget :: Handle ev -> IO TargetAttachToBrowserTarget
targetAttachToBrowserTarget handle = sendReceiveCommandResult handle "Target.attachToBrowserTarget" (Nothing :: Maybe ())

-- | Return type of the 'targetAttachToBrowserTarget' command.
data TargetAttachToBrowserTarget = TargetAttachToBrowserTarget {
  -- | Id assigned to the session.
  targetAttachToBrowserTargetSessionId :: TargetSessionId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToBrowserTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command TargetAttachToBrowserTarget where
   commandName _ = "Target.attachToBrowserTarget"



-- | Parameters of the 'targetCloseTarget' command.
data PTargetCloseTarget = PTargetCloseTarget {
  pTargetCloseTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCloseTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PTargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the 'Target.closeTarget' command.
--   Closes the target. If the target is a page that gets closed too.
--   Parameters: 'PTargetCloseTarget'
targetCloseTarget :: Handle ev -> PTargetCloseTarget -> IO ()
targetCloseTarget handle params = sendReceiveCommand handle "Target.closeTarget" (Just params)


-- | Parameters of the 'targetExposeDevToolsProtocol' command.
data PTargetExposeDevToolsProtocol = PTargetExposeDevToolsProtocol {
  pTargetExposeDevToolsProtocolTargetId :: TargetTargetId,
  -- | Binding name, 'cdp' if not specified.
  pTargetExposeDevToolsProtocolBindingName :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetExposeDevToolsProtocol  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PTargetExposeDevToolsProtocol where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the 'Target.exposeDevToolsProtocol' command.
--   Inject object to the target's main frame that provides a communication
--   channel with browser target.
--   
--   Injected object will be available as `window[bindingName]`.
--   
--   The object has the follwing API:
--   - `binding.send(json)` - a method to send messages over the remote debugging protocol
--   - `binding.onmessage = json => handleMessage(json)` - a callback that will be called for the protocol notifications and command responses.
--   Parameters: 'PTargetExposeDevToolsProtocol'
targetExposeDevToolsProtocol :: Handle ev -> PTargetExposeDevToolsProtocol -> IO ()
targetExposeDevToolsProtocol handle params = sendReceiveCommand handle "Target.exposeDevToolsProtocol" (Just params)


-- | Parameters of the 'targetCreateBrowserContext' command.
data PTargetCreateBrowserContext = PTargetCreateBrowserContext {
  -- | If specified, disposes this context when debugging session disconnects.
  pTargetCreateBrowserContextDisposeOnDetach :: Maybe Bool,
  -- | Proxy server, similar to the one passed to --proxy-server
  pTargetCreateBrowserContextProxyServer :: Maybe String,
  -- | Proxy bypass list, similar to the one passed to --proxy-bypass-list
  pTargetCreateBrowserContextProxyBypassList :: Maybe String,
  -- | An optional list of origins to grant unlimited cross-origin access to.
  --   Parts of the URL other than those constituting origin are ignored.
  pTargetCreateBrowserContextOriginsWithUniversalNetworkAccess :: Maybe [String]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the 'Target.createBrowserContext' command.
--   Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
--   one.
--   Parameters: 'PTargetCreateBrowserContext'
--   Returns: 'TargetCreateBrowserContext'
targetCreateBrowserContext :: Handle ev -> PTargetCreateBrowserContext -> IO TargetCreateBrowserContext
targetCreateBrowserContext handle params = sendReceiveCommandResult handle "Target.createBrowserContext" (Just params)

-- | Return type of the 'targetCreateBrowserContext' command.
data TargetCreateBrowserContext = TargetCreateBrowserContext {
  -- | The id of the context created.
  targetCreateBrowserContextBrowserContextId :: BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command TargetCreateBrowserContext where
   commandName _ = "Target.createBrowserContext"



-- | Function for the 'Target.getBrowserContexts' command.
--   Returns all browser contexts created with `Target.createBrowserContext` method.
--   Returns: 'TargetGetBrowserContexts'
targetGetBrowserContexts :: Handle ev -> IO TargetGetBrowserContexts
targetGetBrowserContexts handle = sendReceiveCommandResult handle "Target.getBrowserContexts" (Nothing :: Maybe ())

-- | Return type of the 'targetGetBrowserContexts' command.
data TargetGetBrowserContexts = TargetGetBrowserContexts {
  -- | An array of browser context ids.
  targetGetBrowserContextsBrowserContextIds :: [BrowserBrowserContextId]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetBrowserContexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command TargetGetBrowserContexts where
   commandName _ = "Target.getBrowserContexts"



-- | Parameters of the 'targetCreateTarget' command.
data PTargetCreateTarget = PTargetCreateTarget {
  -- | The initial URL the page will be navigated to. An empty string indicates about:blank.
  pTargetCreateTargetUrl :: String,
  -- | Frame width in DIP (headless chrome only).
  pTargetCreateTargetWidth :: Maybe Int,
  -- | Frame height in DIP (headless chrome only).
  pTargetCreateTargetHeight :: Maybe Int,
  -- | The browser context to create the page in.
  pTargetCreateTargetBrowserContextId :: Maybe BrowserBrowserContextId,
  -- | Whether BeginFrames for this target will be controlled via DevTools (headless chrome only,
  --   not supported on MacOS yet, false by default).
  pTargetCreateTargetEnableBeginFrameControl :: Maybe Bool,
  -- | Whether to create a new Window or Tab (chrome-only, false by default).
  pTargetCreateTargetNewWindow :: Maybe Bool,
  -- | Whether to create the target in background or foreground (chrome-only,
  --   false by default).
  pTargetCreateTargetBackground :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the 'Target.createTarget' command.
--   Creates a new page.
--   Parameters: 'PTargetCreateTarget'
--   Returns: 'TargetCreateTarget'
targetCreateTarget :: Handle ev -> PTargetCreateTarget -> IO TargetCreateTarget
targetCreateTarget handle params = sendReceiveCommandResult handle "Target.createTarget" (Just params)

-- | Return type of the 'targetCreateTarget' command.
data TargetCreateTarget = TargetCreateTarget {
  -- | The id of the page opened.
  targetCreateTargetTargetId :: TargetTargetId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command TargetCreateTarget where
   commandName _ = "Target.createTarget"



-- | Parameters of the 'targetDetachFromTarget' command.
data PTargetDetachFromTarget = PTargetDetachFromTarget {
  -- | Session to detach.
  pTargetDetachFromTargetSessionId :: Maybe TargetSessionId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDetachFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PTargetDetachFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the 'Target.detachFromTarget' command.
--   Detaches session with given id.
--   Parameters: 'PTargetDetachFromTarget'
targetDetachFromTarget :: Handle ev -> PTargetDetachFromTarget -> IO ()
targetDetachFromTarget handle params = sendReceiveCommand handle "Target.detachFromTarget" (Just params)


-- | Parameters of the 'targetDisposeBrowserContext' command.
data PTargetDisposeBrowserContext = PTargetDisposeBrowserContext {
  pTargetDisposeBrowserContextBrowserContextId :: BrowserBrowserContextId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDisposeBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PTargetDisposeBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the 'Target.disposeBrowserContext' command.
--   Deletes a BrowserContext. All the belonging pages will be closed without calling their
--   beforeunload hooks.
--   Parameters: 'PTargetDisposeBrowserContext'
targetDisposeBrowserContext :: Handle ev -> PTargetDisposeBrowserContext -> IO ()
targetDisposeBrowserContext handle params = sendReceiveCommand handle "Target.disposeBrowserContext" (Just params)


-- | Parameters of the 'targetGetTargetInfo' command.
data PTargetGetTargetInfo = PTargetGetTargetInfo {
  pTargetGetTargetInfoTargetId :: Maybe TargetTargetId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetGetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Target.getTargetInfo' command.
--   Returns information about a target.
--   Parameters: 'PTargetGetTargetInfo'
--   Returns: 'TargetGetTargetInfo'
targetGetTargetInfo :: Handle ev -> PTargetGetTargetInfo -> IO TargetGetTargetInfo
targetGetTargetInfo handle params = sendReceiveCommandResult handle "Target.getTargetInfo" (Just params)

-- | Return type of the 'targetGetTargetInfo' command.
data TargetGetTargetInfo = TargetGetTargetInfo {
  targetGetTargetInfoTargetInfo :: TargetTargetInfo
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command TargetGetTargetInfo where
   commandName _ = "Target.getTargetInfo"



-- | Function for the 'Target.getTargets' command.
--   Retrieves a list of available targets.
--   Returns: 'TargetGetTargets'
targetGetTargets :: Handle ev -> IO TargetGetTargets
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())

-- | Return type of the 'targetGetTargets' command.
data TargetGetTargets = TargetGetTargets {
  -- | The list of targets.
  targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command TargetGetTargets where
   commandName _ = "Target.getTargets"



-- | Parameters of the 'targetSetAutoAttach' command.
data PTargetSetAutoAttach = PTargetSetAutoAttach {
  -- | Whether to auto-attach to related targets.
  pTargetSetAutoAttachAutoAttach :: Bool,
  -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
  --   to run paused targets.
  pTargetSetAutoAttachWaitForDebuggerOnStart :: Bool,
  -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
  --   We plan to make this the default, deprecate non-flattened mode,
  --   and eventually retire it. See crbug.com/991325.
  pTargetSetAutoAttachFlatten :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetAutoAttach  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetSetAutoAttach where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the 'Target.setAutoAttach' command.
--   Controls whether to automatically attach to new targets which are considered to be related to
--   this one. When turned on, attaches to all existing related targets as well. When turned off,
--   automatically detaches from all currently attached targets.
--   This also clears all targets added by `autoAttachRelated` from the list of targets to watch
--   for creation of related targets.
--   Parameters: 'PTargetSetAutoAttach'
targetSetAutoAttach :: Handle ev -> PTargetSetAutoAttach -> IO ()
targetSetAutoAttach handle params = sendReceiveCommand handle "Target.setAutoAttach" (Just params)


-- | Parameters of the 'targetAutoAttachRelated' command.
data PTargetAutoAttachRelated = PTargetAutoAttachRelated {
  pTargetAutoAttachRelatedTargetId :: TargetTargetId,
  -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
  --   to run paused targets.
  pTargetAutoAttachRelatedWaitForDebuggerOnStart :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAutoAttachRelated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PTargetAutoAttachRelated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the 'Target.autoAttachRelated' command.
--   Adds the specified target to the list of targets that will be monitored for any related target
--   creation (such as child frames, child workers and new versions of service worker) and reported
--   through `attachedToTarget`. The specified target is also auto-attached.
--   This cancels the effect of any previous `setAutoAttach` and is also cancelled by subsequent
--   `setAutoAttach`. Only available at the Browser target.
--   Parameters: 'PTargetAutoAttachRelated'
targetAutoAttachRelated :: Handle ev -> PTargetAutoAttachRelated -> IO ()
targetAutoAttachRelated handle params = sendReceiveCommand handle "Target.autoAttachRelated" (Just params)


-- | Parameters of the 'targetSetDiscoverTargets' command.
data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
  -- | Whether to discover available targets.
  pTargetSetDiscoverTargetsDiscover :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetDiscoverTargets  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetDiscoverTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Target.setDiscoverTargets' command.
--   Controls whether to discover available targets and notify via
--   `targetCreated/targetInfoChanged/targetDestroyed` events.
--   Parameters: 'PTargetSetDiscoverTargets'
targetSetDiscoverTargets :: Handle ev -> PTargetSetDiscoverTargets -> IO ()
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)


-- | Parameters of the 'targetSetRemoteLocations' command.
data PTargetSetRemoteLocations = PTargetSetRemoteLocations {
  -- | List of remote locations.
  pTargetSetRemoteLocationsLocations :: [TargetRemoteLocation]
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetRemoteLocations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetRemoteLocations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the 'Target.setRemoteLocations' command.
--   Enables target discovery for the specified locations, when `setDiscoverTargets` was set to
--   `true`.
--   Parameters: 'PTargetSetRemoteLocations'
targetSetRemoteLocations :: Handle ev -> PTargetSetRemoteLocations -> IO ()
targetSetRemoteLocations handle params = sendReceiveCommand handle "Target.setRemoteLocations" (Just params)



