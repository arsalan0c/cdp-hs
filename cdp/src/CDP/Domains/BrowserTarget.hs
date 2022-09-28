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


-- | Type 'Browser.BrowserContextID' .
type BrowserBrowserContextId = String

-- | Type 'Browser.WindowID' .
type BrowserWindowId = Int

-- | The state of the browser window.
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



-- | Browser window bounds information
data BrowserBounds = BrowserBounds {
   browserBoundsLeft :: BrowserBoundsLeft, -- ^ The offset from the left edge of the screen to the window in pixels.
   browserBoundsTop :: BrowserBoundsTop, -- ^ The offset from the top edge of the screen to the window in pixels.
   browserBoundsWidth :: BrowserBoundsWidth, -- ^ The window width in pixels.
   browserBoundsHeight :: BrowserBoundsHeight, -- ^ The window height in pixels.
   browserBoundsWindowState :: BrowserBoundsWindowState -- ^ The window state. Default to normal.
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Type 'Browser.PermissionType' .
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



-- | Type 'Browser.PermissionSetting' .
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



-- | Definition of PermissionDescriptor defined in the Permissions API:
-- https://w3c.github.io/permissions/#dictdef-permissiondescriptor.
data BrowserPermissionDescriptor = BrowserPermissionDescriptor {
   browserPermissionDescriptorName :: BrowserPermissionDescriptorName, -- ^ Name of permission.
See https://cs.chromium.org/chromium/src/third_party/blink/renderer/modules/permissions/permission_descriptor.idl for valid permission names.
   browserPermissionDescriptorSysex :: BrowserPermissionDescriptorSysex, -- ^ For "midi" permission, may also specify sysex control.
   browserPermissionDescriptorUserVisibleOnly :: BrowserPermissionDescriptorUserVisibleOnly, -- ^ For "push" permission, may specify userVisibleOnly.
Note that userVisibleOnly = true is the only currently supported type.
   browserPermissionDescriptorAllowWithoutSanitization :: BrowserPermissionDescriptorAllowWithoutSanitization, -- ^ For "clipboard" permission, may specify allowWithoutSanitization.
   browserPermissionDescriptorPanTiltZoom :: BrowserPermissionDescriptorPanTiltZoom -- ^ For "camera" permission, may specify panTiltZoom.
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserPermissionDescriptor  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  BrowserPermissionDescriptor where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }



-- | Browser command ids used by executeBrowserCommand.
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



-- | Chrome histogram bucket.
data BrowserBucket = BrowserBucket {
   browserBucketLow :: BrowserBucketLow, -- ^ Minimum value (inclusive).
   browserBucketHigh :: BrowserBucketHigh, -- ^ Maximum value (exclusive).
   browserBucketCount :: BrowserBucketCount -- ^ Number of samples.
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserBucket  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 , A.omitNothingFields = True}

instance FromJSON  BrowserBucket where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 13 }



-- | Chrome histogram.
data BrowserHistogram = BrowserHistogram {
   browserHistogramName :: BrowserHistogramName, -- ^ Name.
   browserHistogramSum :: BrowserHistogramSum, -- ^ Sum of sample values.
   browserHistogramCount :: BrowserHistogramCount, -- ^ Total number of samples.
   browserHistogramBuckets :: BrowserHistogramBuckets -- ^ Buckets.
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  BrowserHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }





-- | Type of the 'Browser.downloadWillBegin' event.
data BrowserDownloadWillBegin = BrowserDownloadWillBegin {
   browserDownloadWillBeginFrameId :: BrowserDownloadWillBeginFrameId, -- ^ Id of the frame that caused the download to begin.
   browserDownloadWillBeginGuid :: BrowserDownloadWillBeginGuid, -- ^ Global unique identifier of the download.
   browserDownloadWillBeginUrl :: BrowserDownloadWillBeginUrl, -- ^ URL of the resource being downloaded.
   browserDownloadWillBeginSuggestedFilename :: BrowserDownloadWillBeginSuggestedFilename -- ^ Suggested file name of the resource (the actual name of the file saved on disk may differ).
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadWillBegin  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadWillBegin where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



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
   browserDownloadProgressGuid :: BrowserDownloadProgressGuid, -- ^ Global unique identifier of the download.
   browserDownloadProgressTotalBytes :: BrowserDownloadProgressTotalBytes, -- ^ Total expected bytes to download.
   browserDownloadProgressReceivedBytes :: BrowserDownloadProgressReceivedBytes, -- ^ Total bytes received.
   browserDownloadProgressState :: BrowserDownloadProgressState -- ^ Download status.
} deriving (Generic, Eq, Show, Read)
instance ToJSON BrowserDownloadProgress  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  BrowserDownloadProgress where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





-- | Parameters of the 'browserSetPermission' command.
data PBrowserSetPermission = PBrowserSetPermission {
   pBrowserSetPermissionPermission :: PBrowserSetPermissionPermission, -- ^ Descriptor of permission to override.
   pBrowserSetPermissionSetting :: PBrowserSetPermissionSetting, -- ^ Setting of the permission.
   pBrowserSetPermissionOrigin :: PBrowserSetPermissionOrigin, -- ^ Origin the permission applies to, all origins if not specified.
   pBrowserSetPermissionBrowserContextId :: PBrowserSetPermissionBrowserContextId -- ^ Context to override. When omitted, default browser context is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetPermission  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetPermission where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Browser.setPermission'.
-- Set permission settings for given origin.
-- Parameters: 'PBrowserSetPermission'
browserSetPermission :: Handle ev -> PBrowserSetPermission -> IO (Maybe Error)
browserSetPermission handle params = sendReceiveCommand handle "Browser.setPermission" (Just params)


-- | Parameters of the 'browserGrantPermissions' command.
data PBrowserGrantPermissions = PBrowserGrantPermissions {

   pBrowserGrantPermissionsOrigin :: PBrowserGrantPermissionsOrigin, -- ^ Origin the permission applies to, all origins if not specified.
   pBrowserGrantPermissionsBrowserContextId :: PBrowserGrantPermissionsBrowserContextId -- ^ BrowserContext to override permissions. When omitted, default browser context is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGrantPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserGrantPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Browser.grantPermissions'.
-- Grant specific permissions to the given origin and reject all others.
-- Parameters: 'PBrowserGrantPermissions'
browserGrantPermissions :: Handle ev -> PBrowserGrantPermissions -> IO (Maybe Error)
browserGrantPermissions handle params = sendReceiveCommand handle "Browser.grantPermissions" (Just params)


-- | Parameters of the 'browserResetPermissions' command.
data PBrowserResetPermissions = PBrowserResetPermissions {
   pBrowserResetPermissionsBrowserContextId :: PBrowserResetPermissionsBrowserContextId -- ^ BrowserContext to reset permissions. When omitted, default browser context is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserResetPermissions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PBrowserResetPermissions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Browser.resetPermissions'.
-- Reset all permission management for all origins.
-- Parameters: 'PBrowserResetPermissions'
browserResetPermissions :: Handle ev -> PBrowserResetPermissions -> IO (Maybe Error)
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
   pBrowserSetDownloadBehaviorBehavior :: PBrowserSetDownloadBehaviorBehavior, -- ^ Whether to allow all or deny all download requests, or use default Chrome behavior if
available (otherwise deny). |allowAndName| allows download and names files according to
their dowmload guids.
   pBrowserSetDownloadBehaviorBrowserContextId :: PBrowserSetDownloadBehaviorBrowserContextId, -- ^ BrowserContext to set download behavior. When omitted, default browser context is used.
   pBrowserSetDownloadBehaviorDownloadPath :: PBrowserSetDownloadBehaviorDownloadPath, -- ^ The default path to save downloaded files to. This is required if behavior is set to 'allow'
or 'allowAndName'.
   pBrowserSetDownloadBehaviorEventsEnabled :: PBrowserSetDownloadBehaviorEventsEnabled -- ^ Whether to emit download events (defaults to false).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDownloadBehavior  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDownloadBehavior where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Browser.setDownloadBehavior'.
-- Set the behavior when downloading a file.
-- Parameters: 'PBrowserSetDownloadBehavior'
browserSetDownloadBehavior :: Handle ev -> PBrowserSetDownloadBehavior -> IO (Maybe Error)
browserSetDownloadBehavior handle params = sendReceiveCommand handle "Browser.setDownloadBehavior" (Just params)


-- | Parameters of the 'browserCancelDownload' command.
data PBrowserCancelDownload = PBrowserCancelDownload {
   pBrowserCancelDownloadGuid :: PBrowserCancelDownloadGuid, -- ^ Global unique identifier of the download.
   pBrowserCancelDownloadBrowserContextId :: PBrowserCancelDownloadBrowserContextId -- ^ BrowserContext to perform the action in. When omitted, default browser context is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserCancelDownload  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PBrowserCancelDownload where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'Browser.cancelDownload'.
-- Cancel a download if in progress
-- Parameters: 'PBrowserCancelDownload'
browserCancelDownload :: Handle ev -> PBrowserCancelDownload -> IO (Maybe Error)
browserCancelDownload handle params = sendReceiveCommand handle "Browser.cancelDownload" (Just params)


-- | Function for the command 'Browser.close'.
-- Close browser gracefully.
browserClose :: Handle ev -> IO (Maybe Error)
browserClose handle = sendReceiveCommand handle "Browser.close" (Nothing :: Maybe ())


-- | Function for the command 'Browser.crash'.
-- Crashes browser on the main thread.
browserCrash :: Handle ev -> IO (Maybe Error)
browserCrash handle = sendReceiveCommand handle "Browser.crash" (Nothing :: Maybe ())


-- | Function for the command 'Browser.crashGpuProcess'.
-- Crashes GPU process.
browserCrashGpuProcess :: Handle ev -> IO (Maybe Error)
browserCrashGpuProcess handle = sendReceiveCommand handle "Browser.crashGpuProcess" (Nothing :: Maybe ())


-- | Function for the command 'Browser.getVersion'.
-- Returns version information.
-- Returns: 'BrowserGetVersion'
browserGetVersion :: Handle ev -> IO (Either Error BrowserGetVersion)
browserGetVersion handle = sendReceiveCommandResult handle "Browser.getVersion" (Nothing :: Maybe ())

-- | Return type of the 'browserGetVersion' command.
data BrowserGetVersion = BrowserGetVersion {
   browserGetVersionProtocolVersion :: String, -- ^ Protocol version.
   browserGetVersionProduct :: String, -- ^ Product name.
   browserGetVersionRevision :: String, -- ^ Product revision.
   browserGetVersionUserAgent :: String, -- ^ User-Agent.
   browserGetVersionJsVersion :: String -- ^ V8 version.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetVersion where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 17 }

instance Command BrowserGetVersion where
   commandName _ = "Browser.getVersion"



-- | Function for the command 'Browser.getBrowserCommandLine'.
-- Returns the command line switches for the browser process if, and only if
-- --enable-automation is on the commandline.
-- Returns: 'BrowserGetBrowserCommandLine'
browserGetBrowserCommandLine :: Handle ev -> IO (Either Error BrowserGetBrowserCommandLine)
browserGetBrowserCommandLine handle = sendReceiveCommandResult handle "Browser.getBrowserCommandLine" (Nothing :: Maybe ())

-- | Return type of the 'browserGetBrowserCommandLine' command.
data BrowserGetBrowserCommandLine = BrowserGetBrowserCommandLine {
   browserGetBrowserCommandLineArguments :: [String] -- ^ Commandline parameters
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetBrowserCommandLine where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }

instance Command BrowserGetBrowserCommandLine where
   commandName _ = "Browser.getBrowserCommandLine"



-- | Parameters of the 'browserGetHistograms' command.
data PBrowserGetHistograms = PBrowserGetHistograms {
   pBrowserGetHistogramsQuery :: PBrowserGetHistogramsQuery, -- ^ Requested substring in name. Only histograms which have query as a
substring in their name are extracted. An empty or absent query returns
all histograms.
   pBrowserGetHistogramsDelta :: PBrowserGetHistogramsDelta -- ^ If true, retrieve delta since last call.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistograms  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Browser.getHistograms'.
-- Get Chrome histograms.
-- Parameters: 'PBrowserGetHistograms'
-- Returns: 'BrowserGetHistograms'
browserGetHistograms :: Handle ev -> PBrowserGetHistograms -> IO (Either Error BrowserGetHistograms)
browserGetHistograms handle params = sendReceiveCommandResult handle "Browser.getHistograms" (Just params)

-- | Return type of the 'browserGetHistograms' command.
data BrowserGetHistograms = BrowserGetHistograms {
   browserGetHistogramsHistograms :: [BrowserHistogram] -- ^ Histograms.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistograms where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command BrowserGetHistograms where
   commandName _ = "Browser.getHistograms"



-- | Parameters of the 'browserGetHistogram' command.
data PBrowserGetHistogram = PBrowserGetHistogram {
   pBrowserGetHistogramName :: PBrowserGetHistogramName, -- ^ Requested histogram name.
   pBrowserGetHistogramDelta :: PBrowserGetHistogramDelta -- ^ If true, retrieve delta since last call.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetHistogram  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Browser.getHistogram'.
-- Get a Chrome histogram by name.
-- Parameters: 'PBrowserGetHistogram'
-- Returns: 'BrowserGetHistogram'
browserGetHistogram :: Handle ev -> PBrowserGetHistogram -> IO (Either Error BrowserGetHistogram)
browserGetHistogram handle params = sendReceiveCommandResult handle "Browser.getHistogram" (Just params)

-- | Return type of the 'browserGetHistogram' command.
data BrowserGetHistogram = BrowserGetHistogram {
   browserGetHistogramHistogram :: BrowserHistogram -- ^ Histogram.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetHistogram where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command BrowserGetHistogram where
   commandName _ = "Browser.getHistogram"



-- | Parameters of the 'browserGetWindowBounds' command.
data PBrowserGetWindowBounds = PBrowserGetWindowBounds {
   pBrowserGetWindowBoundsWindowId :: PBrowserGetWindowBoundsWindowId -- ^ Browser window id.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Browser.getWindowBounds'.
-- Get position and size of the browser window.
-- Parameters: 'PBrowserGetWindowBounds'
-- Returns: 'BrowserGetWindowBounds'
browserGetWindowBounds :: Handle ev -> PBrowserGetWindowBounds -> IO (Either Error BrowserGetWindowBounds)
browserGetWindowBounds handle params = sendReceiveCommandResult handle "Browser.getWindowBounds" (Just params)

-- | Return type of the 'browserGetWindowBounds' command.
data BrowserGetWindowBounds = BrowserGetWindowBounds {
   browserGetWindowBoundsBounds :: BrowserBounds -- ^ Bounds information of the window. When window state is 'minimized', the restored window
position and size are returned.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command BrowserGetWindowBounds where
   commandName _ = "Browser.getWindowBounds"



-- | Parameters of the 'browserGetWindowForTarget' command.
data PBrowserGetWindowForTarget = PBrowserGetWindowForTarget {
   pBrowserGetWindowForTargetTargetId :: PBrowserGetWindowForTargetTargetId -- ^ Devtools agent host id. If called as a part of the session, associated targetId is used.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserGetWindowForTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 , A.omitNothingFields = True}

instance FromJSON  PBrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }


-- | Function for the command 'Browser.getWindowForTarget'.
-- Get the browser window that contains the devtools target.
-- Parameters: 'PBrowserGetWindowForTarget'
-- Returns: 'BrowserGetWindowForTarget'
browserGetWindowForTarget :: Handle ev -> PBrowserGetWindowForTarget -> IO (Either Error BrowserGetWindowForTarget)
browserGetWindowForTarget handle params = sendReceiveCommandResult handle "Browser.getWindowForTarget" (Just params)

-- | Return type of the 'browserGetWindowForTarget' command.
data BrowserGetWindowForTarget = BrowserGetWindowForTarget {
   browserGetWindowForTargetWindowId :: BrowserWindowId, -- ^ Browser window id.
   browserGetWindowForTargetBounds :: BrowserBounds -- ^ Bounds information of the window. When window state is 'minimized', the restored window
position and size are returned.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  BrowserGetWindowForTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }

instance Command BrowserGetWindowForTarget where
   commandName _ = "Browser.getWindowForTarget"



-- | Parameters of the 'browserSetWindowBounds' command.
data PBrowserSetWindowBounds = PBrowserSetWindowBounds {
   pBrowserSetWindowBoundsWindowId :: PBrowserSetWindowBoundsWindowId, -- ^ Browser window id.
   pBrowserSetWindowBoundsBounds :: PBrowserSetWindowBoundsBounds -- ^ New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined
with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetWindowBounds  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetWindowBounds where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Browser.setWindowBounds'.
-- Set position and/or size of the browser window.
-- Parameters: 'PBrowserSetWindowBounds'
browserSetWindowBounds :: Handle ev -> PBrowserSetWindowBounds -> IO (Maybe Error)
browserSetWindowBounds handle params = sendReceiveCommand handle "Browser.setWindowBounds" (Just params)


-- | Parameters of the 'browserSetDockTile' command.
data PBrowserSetDockTile = PBrowserSetDockTile {

   pBrowserSetDockTileImage :: PBrowserSetDockTileImage -- ^ Png encoded image. (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserSetDockTile  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PBrowserSetDockTile where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'Browser.setDockTile'.
-- Set dock tile details, platform-specific.
-- Parameters: 'PBrowserSetDockTile'
browserSetDockTile :: Handle ev -> PBrowserSetDockTile -> IO (Maybe Error)
browserSetDockTile handle params = sendReceiveCommand handle "Browser.setDockTile" (Just params)


-- | Parameters of the 'browserExecuteBrowserCommand' command.
data PBrowserExecuteBrowserCommand = PBrowserExecuteBrowserCommand {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PBrowserExecuteBrowserCommand  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PBrowserExecuteBrowserCommand where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Browser.executeBrowserCommand'.
-- Invoke custom browser commands used by telemetry.
-- Parameters: 'PBrowserExecuteBrowserCommand'
browserExecuteBrowserCommand :: Handle ev -> PBrowserExecuteBrowserCommand -> IO (Maybe Error)
browserExecuteBrowserCommand handle params = sendReceiveCommand handle "Browser.executeBrowserCommand" (Just params)



-- | Type 'Target.TargetID' .
type TargetTargetId = String

-- | Unique identifier of attached debugging session.
type TargetSessionId = String

-- | Type 'Target.TargetInfo' .
data TargetTargetInfo = TargetTargetInfo {




   targetTargetInfoAttached :: TargetTargetInfoAttached, -- ^ Whether the target has an attached client.
   targetTargetInfoOpenerId :: TargetTargetInfoOpenerId, -- ^ Opener target Id
   targetTargetInfoCanAccessOpener :: TargetTargetInfoCanAccessOpener, -- ^ Whether the target has access to the originating window.
   targetTargetInfoOpenerFrameId :: TargetTargetInfoOpenerFrameId, -- ^ Frame id of originating window (is only set if target has an opener).

} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }



-- | Type 'Target.RemoteLocation' .
data TargetRemoteLocation = TargetRemoteLocation {


} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetRemoteLocation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  TargetRemoteLocation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }





-- | Type of the 'Target.attachedToTarget' event.
data TargetAttachedToTarget = TargetAttachedToTarget {
   targetAttachedToTargetSessionId :: TargetAttachedToTargetSessionId, -- ^ Identifier assigned to the session used to send/receive messages.


} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetAttachedToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  TargetAttachedToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }



-- | Type of the 'Target.detachedFromTarget' event.
data TargetDetachedFromTarget = TargetDetachedFromTarget {
   targetDetachedFromTargetSessionId :: TargetDetachedFromTargetSessionId -- ^ Detached session identifier.
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetDetachedFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  TargetDetachedFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }



-- | Type of the 'Target.receivedMessageFromTarget' event.
data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget {
   targetReceivedMessageFromTargetSessionId :: TargetReceivedMessageFromTargetSessionId, -- ^ Identifier of a session which sends a message.

} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetReceivedMessageFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 , A.omitNothingFields = True}

instance FromJSON  TargetReceivedMessageFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }



-- | Type of the 'Target.targetCreated' event.
data TargetTargetCreated = TargetTargetCreated {
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCreated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCreated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Target.targetDestroyed' event.
data TargetTargetDestroyed = TargetTargetDestroyed {
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetDestroyed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  TargetTargetDestroyed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }



-- | Type of the 'Target.targetCrashed' event.
data TargetTargetCrashed = TargetTargetCrashed {

   targetTargetCrashedStatus :: TargetTargetCrashedStatus, -- ^ Termination status type.
   targetTargetCrashedErrorCode :: TargetTargetCrashedErrorCode -- ^ Termination error code.
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetCrashed  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  TargetTargetCrashed where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }



-- | Type of the 'Target.targetInfoChanged' event.
data TargetTargetInfoChanged = TargetTargetInfoChanged {
} deriving (Generic, Eq, Show, Read)
instance ToJSON TargetTargetInfoChanged  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  TargetTargetInfoChanged where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }





-- | Parameters of the 'targetActivateTarget' command.
data PTargetActivateTarget = PTargetActivateTarget {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetActivateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetActivateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Target.activateTarget'.
-- Activates (focuses) the target.
-- Parameters: 'PTargetActivateTarget'
targetActivateTarget :: Handle ev -> PTargetActivateTarget -> IO (Maybe Error)
targetActivateTarget handle params = sendReceiveCommand handle "Target.activateTarget" (Just params)


-- | Parameters of the 'targetAttachToTarget' command.
data PTargetAttachToTarget = PTargetAttachToTarget {

   pTargetAttachToTargetFlatten :: PTargetAttachToTargetFlatten -- ^ Enables "flat" access to the session via specifying sessionId attribute in the commands.
We plan to make this the default, deprecate non-flattened mode,
and eventually retire it. See crbug.com/991325.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAttachToTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 , A.omitNothingFields = True}

instance FromJSON  PTargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }


-- | Function for the command 'Target.attachToTarget'.
-- Attaches to the target with given id.
-- Parameters: 'PTargetAttachToTarget'
-- Returns: 'TargetAttachToTarget'
targetAttachToTarget :: Handle ev -> PTargetAttachToTarget -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget handle params = sendReceiveCommandResult handle "Target.attachToTarget" (Just params)

-- | Return type of the 'targetAttachToTarget' command.
data TargetAttachToTarget = TargetAttachToTarget {
   targetAttachToTargetSessionId :: TargetSessionId -- ^ Id assigned to the session.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }

instance Command TargetAttachToTarget where
   commandName _ = "Target.attachToTarget"



-- | Function for the command 'Target.attachToBrowserTarget'.
-- Attaches to the browser target, only uses flat sessionId mode.
-- Returns: 'TargetAttachToBrowserTarget'
targetAttachToBrowserTarget :: Handle ev -> IO (Either Error TargetAttachToBrowserTarget)
targetAttachToBrowserTarget handle = sendReceiveCommandResult handle "Target.attachToBrowserTarget" (Nothing :: Maybe ())

-- | Return type of the 'targetAttachToBrowserTarget' command.
data TargetAttachToBrowserTarget = TargetAttachToBrowserTarget {
   targetAttachToBrowserTargetSessionId :: TargetSessionId -- ^ Id assigned to the session.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetAttachToBrowserTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }

instance Command TargetAttachToBrowserTarget where
   commandName _ = "Target.attachToBrowserTarget"



-- | Parameters of the 'targetCloseTarget' command.
data PTargetCloseTarget = PTargetCloseTarget {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCloseTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  PTargetCloseTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }


-- | Function for the command 'Target.closeTarget'.
-- Closes the target. If the target is a page that gets closed too.
-- Parameters: 'PTargetCloseTarget'
targetCloseTarget :: Handle ev -> PTargetCloseTarget -> IO (Maybe Error)
targetCloseTarget handle params = sendReceiveCommand handle "Target.closeTarget" (Just params)


-- | Parameters of the 'targetExposeDevToolsProtocol' command.
data PTargetExposeDevToolsProtocol = PTargetExposeDevToolsProtocol {

   pTargetExposeDevToolsProtocolBindingName :: PTargetExposeDevToolsProtocolBindingName -- ^ Binding name, 'cdp' if not specified.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetExposeDevToolsProtocol  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 , A.omitNothingFields = True}

instance FromJSON  PTargetExposeDevToolsProtocol where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 29 }


-- | Function for the command 'Target.exposeDevToolsProtocol'.
-- Inject object to the target's main frame that provides a communication
-- channel with browser target.
-- 
-- Injected object will be available as `window[bindingName]`.
-- 
-- The object has the follwing API:
-- - `binding.send(json)` - a method to send messages over the remote debugging protocol
-- - `binding.onmessage = json => handleMessage(json)` - a callback that will be called for the protocol notifications and command responses.
-- Parameters: 'PTargetExposeDevToolsProtocol'
targetExposeDevToolsProtocol :: Handle ev -> PTargetExposeDevToolsProtocol -> IO (Maybe Error)
targetExposeDevToolsProtocol handle params = sendReceiveCommand handle "Target.exposeDevToolsProtocol" (Just params)


-- | Parameters of the 'targetCreateBrowserContext' command.
data PTargetCreateBrowserContext = PTargetCreateBrowserContext {
   pTargetCreateBrowserContextDisposeOnDetach :: PTargetCreateBrowserContextDisposeOnDetach, -- ^ If specified, disposes this context when debugging session disconnects.
   pTargetCreateBrowserContextProxyServer :: PTargetCreateBrowserContextProxyServer, -- ^ Proxy server, similar to the one passed to --proxy-server
   pTargetCreateBrowserContextProxyBypassList :: PTargetCreateBrowserContextProxyBypassList, -- ^ Proxy bypass list, similar to the one passed to --proxy-bypass-list
   pTargetCreateBrowserContextOriginsWithUniversalNetworkAccess :: PTargetCreateBrowserContextOriginsWithUniversalNetworkAccess -- ^ An optional list of origins to grant unlimited cross-origin access to.
Parts of the URL other than those constituting origin are ignored.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 27 }


-- | Function for the command 'Target.createBrowserContext'.
-- Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
-- one.
-- Parameters: 'PTargetCreateBrowserContext'
-- Returns: 'TargetCreateBrowserContext'
targetCreateBrowserContext :: Handle ev -> PTargetCreateBrowserContext -> IO (Either Error TargetCreateBrowserContext)
targetCreateBrowserContext handle params = sendReceiveCommandResult handle "Target.createBrowserContext" (Just params)

-- | Return type of the 'targetCreateBrowserContext' command.
data TargetCreateBrowserContext = TargetCreateBrowserContext {
   targetCreateBrowserContextBrowserContextId :: BrowserBrowserContextId -- ^ The id of the context created.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 26 }

instance Command TargetCreateBrowserContext where
   commandName _ = "Target.createBrowserContext"



-- | Function for the command 'Target.getBrowserContexts'.
-- Returns all browser contexts created with `Target.createBrowserContext` method.
-- Returns: 'TargetGetBrowserContexts'
targetGetBrowserContexts :: Handle ev -> IO (Either Error TargetGetBrowserContexts)
targetGetBrowserContexts handle = sendReceiveCommandResult handle "Target.getBrowserContexts" (Nothing :: Maybe ())

-- | Return type of the 'targetGetBrowserContexts' command.
data TargetGetBrowserContexts = TargetGetBrowserContexts {
   targetGetBrowserContextsBrowserContextIds :: [BrowserBrowserContextId] -- ^ An array of browser context ids.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetBrowserContexts where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }

instance Command TargetGetBrowserContexts where
   commandName _ = "Target.getBrowserContexts"



-- | Parameters of the 'targetCreateTarget' command.
data PTargetCreateTarget = PTargetCreateTarget {
   pTargetCreateTargetUrl :: PTargetCreateTargetUrl, -- ^ The initial URL the page will be navigated to. An empty string indicates about:blank.
   pTargetCreateTargetWidth :: PTargetCreateTargetWidth, -- ^ Frame width in DIP (headless chrome only).
   pTargetCreateTargetHeight :: PTargetCreateTargetHeight, -- ^ Frame height in DIP (headless chrome only).
   pTargetCreateTargetBrowserContextId :: PTargetCreateTargetBrowserContextId, -- ^ The browser context to create the page in.
   pTargetCreateTargetEnableBeginFrameControl :: PTargetCreateTargetEnableBeginFrameControl, -- ^ Whether BeginFrames for this target will be controlled via DevTools (headless chrome only,
not supported on MacOS yet, false by default).
   pTargetCreateTargetNewWindow :: PTargetCreateTargetNewWindow, -- ^ Whether to create a new Window or Tab (chrome-only, false by default).
   pTargetCreateTargetBackground :: PTargetCreateTargetBackground -- ^ Whether to create the target in background or foreground (chrome-only,
false by default).
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetCreateTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 , A.omitNothingFields = True}

instance FromJSON  PTargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }


-- | Function for the command 'Target.createTarget'.
-- Creates a new page.
-- Parameters: 'PTargetCreateTarget'
-- Returns: 'TargetCreateTarget'
targetCreateTarget :: Handle ev -> PTargetCreateTarget -> IO (Either Error TargetCreateTarget)
targetCreateTarget handle params = sendReceiveCommandResult handle "Target.createTarget" (Just params)

-- | Return type of the 'targetCreateTarget' command.
data TargetCreateTarget = TargetCreateTarget {
   targetCreateTargetTargetId :: TargetTargetId -- ^ The id of the page opened.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetCreateTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }

instance Command TargetCreateTarget where
   commandName _ = "Target.createTarget"



-- | Parameters of the 'targetDetachFromTarget' command.
data PTargetDetachFromTarget = PTargetDetachFromTarget {
   pTargetDetachFromTargetSessionId :: PTargetDetachFromTargetSessionId -- ^ Session to detach.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDetachFromTarget  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PTargetDetachFromTarget where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'Target.detachFromTarget'.
-- Detaches session with given id.
-- Parameters: 'PTargetDetachFromTarget'
targetDetachFromTarget :: Handle ev -> PTargetDetachFromTarget -> IO (Maybe Error)
targetDetachFromTarget handle params = sendReceiveCommand handle "Target.detachFromTarget" (Just params)


-- | Parameters of the 'targetDisposeBrowserContext' command.
data PTargetDisposeBrowserContext = PTargetDisposeBrowserContext {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetDisposeBrowserContext  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 , A.omitNothingFields = True}

instance FromJSON  PTargetDisposeBrowserContext where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 28 }


-- | Function for the command 'Target.disposeBrowserContext'.
-- Deletes a BrowserContext. All the belonging pages will be closed without calling their
-- beforeunload hooks.
-- Parameters: 'PTargetDisposeBrowserContext'
targetDisposeBrowserContext :: Handle ev -> PTargetDisposeBrowserContext -> IO (Maybe Error)
targetDisposeBrowserContext handle params = sendReceiveCommand handle "Target.disposeBrowserContext" (Just params)


-- | Parameters of the 'targetGetTargetInfo' command.
data PTargetGetTargetInfo = PTargetGetTargetInfo {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetGetTargetInfo  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Target.getTargetInfo'.
-- Returns information about a target.
-- Parameters: 'PTargetGetTargetInfo'
-- Returns: 'TargetGetTargetInfo'
targetGetTargetInfo :: Handle ev -> PTargetGetTargetInfo -> IO (Either Error TargetGetTargetInfo)
targetGetTargetInfo handle params = sendReceiveCommandResult handle "Target.getTargetInfo" (Just params)

-- | Return type of the 'targetGetTargetInfo' command.
data TargetGetTargetInfo = TargetGetTargetInfo {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargetInfo where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 19 }

instance Command TargetGetTargetInfo where
   commandName _ = "Target.getTargetInfo"



-- | Function for the command 'Target.getTargets'.
-- Retrieves a list of available targets.
-- Returns: 'TargetGetTargets'
targetGetTargets :: Handle ev -> IO (Either Error TargetGetTargets)
targetGetTargets handle = sendReceiveCommandResult handle "Target.getTargets" (Nothing :: Maybe ())

-- | Return type of the 'targetGetTargets' command.
data TargetGetTargets = TargetGetTargets {
   targetGetTargetsTargetInfos :: [TargetTargetInfo] -- ^ The list of targets.
} deriving (Generic, Eq, Show, Read)

instance FromJSON  TargetGetTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 16 }

instance Command TargetGetTargets where
   commandName _ = "Target.getTargets"



-- | Parameters of the 'targetSetAutoAttach' command.
data PTargetSetAutoAttach = PTargetSetAutoAttach {
   pTargetSetAutoAttachAutoAttach :: PTargetSetAutoAttachAutoAttach, -- ^ Whether to auto-attach to related targets.
   pTargetSetAutoAttachWaitForDebuggerOnStart :: PTargetSetAutoAttachWaitForDebuggerOnStart, -- ^ Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
to run paused targets.
   pTargetSetAutoAttachFlatten :: PTargetSetAutoAttachFlatten -- ^ Enables "flat" access to the session via specifying sessionId attribute in the commands.
We plan to make this the default, deprecate non-flattened mode,
and eventually retire it. See crbug.com/991325.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetAutoAttach  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 , A.omitNothingFields = True}

instance FromJSON  PTargetSetAutoAttach where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 20 }


-- | Function for the command 'Target.setAutoAttach'.
-- Controls whether to automatically attach to new targets which are considered to be related to
-- this one. When turned on, attaches to all existing related targets as well. When turned off,
-- automatically detaches from all currently attached targets.
-- This also clears all targets added by `autoAttachRelated` from the list of targets to watch
-- for creation of related targets.
-- Parameters: 'PTargetSetAutoAttach'
targetSetAutoAttach :: Handle ev -> PTargetSetAutoAttach -> IO (Maybe Error)
targetSetAutoAttach handle params = sendReceiveCommand handle "Target.setAutoAttach" (Just params)


-- | Parameters of the 'targetAutoAttachRelated' command.
data PTargetAutoAttachRelated = PTargetAutoAttachRelated {

   pTargetAutoAttachRelatedWaitForDebuggerOnStart :: PTargetAutoAttachRelatedWaitForDebuggerOnStart -- ^ Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
to run paused targets.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetAutoAttachRelated  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PTargetAutoAttachRelated where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'Target.autoAttachRelated'.
-- Adds the specified target to the list of targets that will be monitored for any related target
-- creation (such as child frames, child workers and new versions of service worker) and reported
-- through `attachedToTarget`. The specified target is also auto-attached.
-- This cancels the effect of any previous `setAutoAttach` and is also cancelled by subsequent
-- `setAutoAttach`. Only available at the Browser target.
-- Parameters: 'PTargetAutoAttachRelated'
targetAutoAttachRelated :: Handle ev -> PTargetAutoAttachRelated -> IO (Maybe Error)
targetAutoAttachRelated handle params = sendReceiveCommand handle "Target.autoAttachRelated" (Just params)


-- | Parameters of the 'targetSetDiscoverTargets' command.
data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets {
   pTargetSetDiscoverTargetsDiscover :: PTargetSetDiscoverTargetsDiscover -- ^ Whether to discover available targets.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetDiscoverTargets  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetDiscoverTargets where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Target.setDiscoverTargets'.
-- Controls whether to discover available targets and notify via
-- `targetCreated/targetInfoChanged/targetDestroyed` events.
-- Parameters: 'PTargetSetDiscoverTargets'
targetSetDiscoverTargets :: Handle ev -> PTargetSetDiscoverTargets -> IO (Maybe Error)
targetSetDiscoverTargets handle params = sendReceiveCommand handle "Target.setDiscoverTargets" (Just params)


-- | Parameters of the 'targetSetRemoteLocations' command.
data PTargetSetRemoteLocations = PTargetSetRemoteLocations {
   pTargetSetRemoteLocationsLocations :: PTargetSetRemoteLocationsLocations -- ^ List of remote locations.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PTargetSetRemoteLocations  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PTargetSetRemoteLocations where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'Target.setRemoteLocations'.
-- Enables target discovery for the specified locations, when `setDiscoverTargets` was set to
-- `true`.
-- Parameters: 'PTargetSetRemoteLocations'
targetSetRemoteLocations :: Handle ev -> PTargetSetRemoteLocations -> IO (Maybe Error)
targetSetRemoteLocations handle params = sendReceiveCommand handle "Target.setRemoteLocations" (Just params)



