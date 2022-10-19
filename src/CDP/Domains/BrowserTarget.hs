{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= Browser

The Browser domain defines methods and events for browser managing.
= Target

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils


import CDP.Domains.DOMPageNetworkEmulationSecurity as DOMPageNetworkEmulationSecurity


-- | Type 'Browser.BrowserContextID'.
type BrowserBrowserContextID = String

-- | Type 'Browser.WindowID'.
type BrowserWindowID = Int

-- | Type 'Browser.WindowState'.
--   The state of the browser window.
data BrowserWindowState = BrowserWindowStateNormal | BrowserWindowStateMinimized | BrowserWindowStateMaximized | BrowserWindowStateFullscreen
  deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserWindowState where
  parseJSON = A.withText "BrowserWindowState" $ \v -> case v of
    "normal" -> pure BrowserWindowStateNormal
    "minimized" -> pure BrowserWindowStateMinimized
    "maximized" -> pure BrowserWindowStateMaximized
    "fullscreen" -> pure BrowserWindowStateFullscreen
    "_" -> fail "failed to parse BrowserWindowState"
instance ToJSON BrowserWindowState where
  toJSON v = A.String $ case v of
    BrowserWindowStateNormal -> "normal"
    BrowserWindowStateMinimized -> "minimized"
    BrowserWindowStateMaximized -> "maximized"
    BrowserWindowStateFullscreen -> "fullscreen"

-- | Type 'Browser.Bounds'.
--   Browser window bounds information
data BrowserBounds = BrowserBounds
  {
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
  }
  deriving (Eq, Show)
instance FromJSON BrowserBounds where
  parseJSON = A.withObject "BrowserBounds" $ \o -> BrowserBounds
    <$> o A..:? "left"
    <*> o A..:? "top"
    <*> o A..:? "width"
    <*> o A..:? "height"
    <*> o A..:? "windowState"
instance ToJSON BrowserBounds where
  toJSON p = A.object $ catMaybes [
    ("left" A..=) <$> (browserBoundsLeft p),
    ("top" A..=) <$> (browserBoundsTop p),
    ("width" A..=) <$> (browserBoundsWidth p),
    ("height" A..=) <$> (browserBoundsHeight p),
    ("windowState" A..=) <$> (browserBoundsWindowState p)
    ]

-- | Type 'Browser.PermissionType'.
data BrowserPermissionType = BrowserPermissionTypeAccessibilityEvents | BrowserPermissionTypeAudioCapture | BrowserPermissionTypeBackgroundSync | BrowserPermissionTypeBackgroundFetch | BrowserPermissionTypeClipboardReadWrite | BrowserPermissionTypeClipboardSanitizedWrite | BrowserPermissionTypeDisplayCapture | BrowserPermissionTypeDurableStorage | BrowserPermissionTypeFlash | BrowserPermissionTypeGeolocation | BrowserPermissionTypeMidi | BrowserPermissionTypeMidiSysex | BrowserPermissionTypeNfc | BrowserPermissionTypeNotifications | BrowserPermissionTypePaymentHandler | BrowserPermissionTypePeriodicBackgroundSync | BrowserPermissionTypeProtectedMediaIdentifier | BrowserPermissionTypeSensors | BrowserPermissionTypeVideoCapture | BrowserPermissionTypeVideoCapturePanTiltZoom | BrowserPermissionTypeIdleDetection | BrowserPermissionTypeWakeLockScreen | BrowserPermissionTypeWakeLockSystem
  deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserPermissionType where
  parseJSON = A.withText "BrowserPermissionType" $ \v -> case v of
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
    "_" -> fail "failed to parse BrowserPermissionType"
instance ToJSON BrowserPermissionType where
  toJSON v = A.String $ case v of
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
  parseJSON = A.withText "BrowserPermissionSetting" $ \v -> case v of
    "granted" -> pure BrowserPermissionSettingGranted
    "denied" -> pure BrowserPermissionSettingDenied
    "prompt" -> pure BrowserPermissionSettingPrompt
    "_" -> fail "failed to parse BrowserPermissionSetting"
instance ToJSON BrowserPermissionSetting where
  toJSON v = A.String $ case v of
    BrowserPermissionSettingGranted -> "granted"
    BrowserPermissionSettingDenied -> "denied"
    BrowserPermissionSettingPrompt -> "prompt"

-- | Type 'Browser.PermissionDescriptor'.
--   Definition of PermissionDescriptor defined in the Permissions API:
--   https://w3c.github.io/permissions/#dictdef-permissiondescriptor.
data BrowserPermissionDescriptor = BrowserPermissionDescriptor
  {
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
  }
  deriving (Eq, Show)
instance FromJSON BrowserPermissionDescriptor where
  parseJSON = A.withObject "BrowserPermissionDescriptor" $ \o -> BrowserPermissionDescriptor
    <$> o A..: "name"
    <*> o A..:? "sysex"
    <*> o A..:? "userVisibleOnly"
    <*> o A..:? "allowWithoutSanitization"
    <*> o A..:? "panTiltZoom"
instance ToJSON BrowserPermissionDescriptor where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (browserPermissionDescriptorName p),
    ("sysex" A..=) <$> (browserPermissionDescriptorSysex p),
    ("userVisibleOnly" A..=) <$> (browserPermissionDescriptorUserVisibleOnly p),
    ("allowWithoutSanitization" A..=) <$> (browserPermissionDescriptorAllowWithoutSanitization p),
    ("panTiltZoom" A..=) <$> (browserPermissionDescriptorPanTiltZoom p)
    ]

-- | Type 'Browser.BrowserCommandId'.
--   Browser command ids used by executeBrowserCommand.
data BrowserBrowserCommandId = BrowserBrowserCommandIdOpenTabSearch | BrowserBrowserCommandIdCloseTabSearch
  deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserBrowserCommandId where
  parseJSON = A.withText "BrowserBrowserCommandId" $ \v -> case v of
    "openTabSearch" -> pure BrowserBrowserCommandIdOpenTabSearch
    "closeTabSearch" -> pure BrowserBrowserCommandIdCloseTabSearch
    "_" -> fail "failed to parse BrowserBrowserCommandId"
instance ToJSON BrowserBrowserCommandId where
  toJSON v = A.String $ case v of
    BrowserBrowserCommandIdOpenTabSearch -> "openTabSearch"
    BrowserBrowserCommandIdCloseTabSearch -> "closeTabSearch"

-- | Type 'Browser.Bucket'.
--   Chrome histogram bucket.
data BrowserBucket = BrowserBucket
  {
    -- | Minimum value (inclusive).
    browserBucketLow :: Int,
    -- | Maximum value (exclusive).
    browserBucketHigh :: Int,
    -- | Number of samples.
    browserBucketCount :: Int
  }
  deriving (Eq, Show)
instance FromJSON BrowserBucket where
  parseJSON = A.withObject "BrowserBucket" $ \o -> BrowserBucket
    <$> o A..: "low"
    <*> o A..: "high"
    <*> o A..: "count"
instance ToJSON BrowserBucket where
  toJSON p = A.object $ catMaybes [
    ("low" A..=) <$> Just (browserBucketLow p),
    ("high" A..=) <$> Just (browserBucketHigh p),
    ("count" A..=) <$> Just (browserBucketCount p)
    ]

-- | Type 'Browser.Histogram'.
--   Chrome histogram.
data BrowserHistogram = BrowserHistogram
  {
    -- | Name.
    browserHistogramName :: String,
    -- | Sum of sample values.
    browserHistogramSum :: Int,
    -- | Total number of samples.
    browserHistogramCount :: Int,
    -- | Buckets.
    browserHistogramBuckets :: [BrowserBucket]
  }
  deriving (Eq, Show)
instance FromJSON BrowserHistogram where
  parseJSON = A.withObject "BrowserHistogram" $ \o -> BrowserHistogram
    <$> o A..: "name"
    <*> o A..: "sum"
    <*> o A..: "count"
    <*> o A..: "buckets"
instance ToJSON BrowserHistogram where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (browserHistogramName p),
    ("sum" A..=) <$> Just (browserHistogramSum p),
    ("count" A..=) <$> Just (browserHistogramCount p),
    ("buckets" A..=) <$> Just (browserHistogramBuckets p)
    ]

-- | Type of the 'Browser.downloadWillBegin' event.
data BrowserDownloadWillBegin = BrowserDownloadWillBegin
  {
    -- | Id of the frame that caused the download to begin.
    browserDownloadWillBeginFrameId :: DOMPageNetworkEmulationSecurity.PageFrameId,
    -- | Global unique identifier of the download.
    browserDownloadWillBeginGuid :: String,
    -- | URL of the resource being downloaded.
    browserDownloadWillBeginUrl :: String,
    -- | Suggested file name of the resource (the actual name of the file saved on disk may differ).
    browserDownloadWillBeginSuggestedFilename :: String
  }
  deriving (Eq, Show)
instance FromJSON BrowserDownloadWillBegin where
  parseJSON = A.withObject "BrowserDownloadWillBegin" $ \o -> BrowserDownloadWillBegin
    <$> o A..: "frameId"
    <*> o A..: "guid"
    <*> o A..: "url"
    <*> o A..: "suggestedFilename"
instance Event BrowserDownloadWillBegin where
  eventName _ = "Browser.downloadWillBegin"

-- | Type of the 'Browser.downloadProgress' event.
data BrowserDownloadProgressState = BrowserDownloadProgressStateInProgress | BrowserDownloadProgressStateCompleted | BrowserDownloadProgressStateCanceled
  deriving (Ord, Eq, Show, Read)
instance FromJSON BrowserDownloadProgressState where
  parseJSON = A.withText "BrowserDownloadProgressState" $ \v -> case v of
    "inProgress" -> pure BrowserDownloadProgressStateInProgress
    "completed" -> pure BrowserDownloadProgressStateCompleted
    "canceled" -> pure BrowserDownloadProgressStateCanceled
    "_" -> fail "failed to parse BrowserDownloadProgressState"
instance ToJSON BrowserDownloadProgressState where
  toJSON v = A.String $ case v of
    BrowserDownloadProgressStateInProgress -> "inProgress"
    BrowserDownloadProgressStateCompleted -> "completed"
    BrowserDownloadProgressStateCanceled -> "canceled"
data BrowserDownloadProgress = BrowserDownloadProgress
  {
    -- | Global unique identifier of the download.
    browserDownloadProgressGuid :: String,
    -- | Total expected bytes to download.
    browserDownloadProgressTotalBytes :: Double,
    -- | Total bytes received.
    browserDownloadProgressReceivedBytes :: Double,
    -- | Download status.
    browserDownloadProgressState :: BrowserDownloadProgressState
  }
  deriving (Eq, Show)
instance FromJSON BrowserDownloadProgress where
  parseJSON = A.withObject "BrowserDownloadProgress" $ \o -> BrowserDownloadProgress
    <$> o A..: "guid"
    <*> o A..: "totalBytes"
    <*> o A..: "receivedBytes"
    <*> o A..: "state"
instance Event BrowserDownloadProgress where
  eventName _ = "Browser.downloadProgress"

-- | Set permission settings for given origin.

-- | Parameters of the 'Browser.setPermission' command.
data PBrowserSetPermission = PBrowserSetPermission
  {
    -- | Descriptor of permission to override.
    pBrowserSetPermissionPermission :: BrowserPermissionDescriptor,
    -- | Setting of the permission.
    pBrowserSetPermissionSetting :: BrowserPermissionSetting,
    -- | Origin the permission applies to, all origins if not specified.
    pBrowserSetPermissionOrigin :: Maybe String,
    -- | Context to override. When omitted, default browser context is used.
    pBrowserSetPermissionBrowserContextId :: Maybe BrowserBrowserContextID
  }
  deriving (Eq, Show)
pBrowserSetPermission
  -- | Descriptor of permission to override.
  :: BrowserPermissionDescriptor
  -- | Setting of the permission.
  -> BrowserPermissionSetting
  -> PBrowserSetPermission
pBrowserSetPermission
  arg_pBrowserSetPermissionPermission
  arg_pBrowserSetPermissionSetting
  = PBrowserSetPermission
    arg_pBrowserSetPermissionPermission
    arg_pBrowserSetPermissionSetting
    Nothing
    Nothing
instance ToJSON PBrowserSetPermission where
  toJSON p = A.object $ catMaybes [
    ("permission" A..=) <$> Just (pBrowserSetPermissionPermission p),
    ("setting" A..=) <$> Just (pBrowserSetPermissionSetting p),
    ("origin" A..=) <$> (pBrowserSetPermissionOrigin p),
    ("browserContextId" A..=) <$> (pBrowserSetPermissionBrowserContextId p)
    ]
instance Command PBrowserSetPermission where
  type CommandResponse PBrowserSetPermission = ()
  commandName _ = "Browser.setPermission"
  fromJSON = const . A.Success . const ()

-- | Grant specific permissions to the given origin and reject all others.

-- | Parameters of the 'Browser.grantPermissions' command.
data PBrowserGrantPermissions = PBrowserGrantPermissions
  {
    pBrowserGrantPermissionsPermissions :: [BrowserPermissionType],
    -- | Origin the permission applies to, all origins if not specified.
    pBrowserGrantPermissionsOrigin :: Maybe String,
    -- | BrowserContext to override permissions. When omitted, default browser context is used.
    pBrowserGrantPermissionsBrowserContextId :: Maybe BrowserBrowserContextID
  }
  deriving (Eq, Show)
pBrowserGrantPermissions
  :: [BrowserPermissionType]
  -> PBrowserGrantPermissions
pBrowserGrantPermissions
  arg_pBrowserGrantPermissionsPermissions
  = PBrowserGrantPermissions
    arg_pBrowserGrantPermissionsPermissions
    Nothing
    Nothing
instance ToJSON PBrowserGrantPermissions where
  toJSON p = A.object $ catMaybes [
    ("permissions" A..=) <$> Just (pBrowserGrantPermissionsPermissions p),
    ("origin" A..=) <$> (pBrowserGrantPermissionsOrigin p),
    ("browserContextId" A..=) <$> (pBrowserGrantPermissionsBrowserContextId p)
    ]
instance Command PBrowserGrantPermissions where
  type CommandResponse PBrowserGrantPermissions = ()
  commandName _ = "Browser.grantPermissions"
  fromJSON = const . A.Success . const ()

-- | Reset all permission management for all origins.

-- | Parameters of the 'Browser.resetPermissions' command.
data PBrowserResetPermissions = PBrowserResetPermissions
  {
    -- | BrowserContext to reset permissions. When omitted, default browser context is used.
    pBrowserResetPermissionsBrowserContextId :: Maybe BrowserBrowserContextID
  }
  deriving (Eq, Show)
pBrowserResetPermissions
  :: PBrowserResetPermissions
pBrowserResetPermissions
  = PBrowserResetPermissions
    Nothing
instance ToJSON PBrowserResetPermissions where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> (pBrowserResetPermissionsBrowserContextId p)
    ]
instance Command PBrowserResetPermissions where
  type CommandResponse PBrowserResetPermissions = ()
  commandName _ = "Browser.resetPermissions"
  fromJSON = const . A.Success . const ()

-- | Set the behavior when downloading a file.

-- | Parameters of the 'Browser.setDownloadBehavior' command.
data PBrowserSetDownloadBehaviorBehavior = PBrowserSetDownloadBehaviorBehaviorDeny | PBrowserSetDownloadBehaviorBehaviorAllow | PBrowserSetDownloadBehaviorBehaviorAllowAndName | PBrowserSetDownloadBehaviorBehaviorDefault
  deriving (Ord, Eq, Show, Read)
instance FromJSON PBrowserSetDownloadBehaviorBehavior where
  parseJSON = A.withText "PBrowserSetDownloadBehaviorBehavior" $ \v -> case v of
    "deny" -> pure PBrowserSetDownloadBehaviorBehaviorDeny
    "allow" -> pure PBrowserSetDownloadBehaviorBehaviorAllow
    "allowAndName" -> pure PBrowserSetDownloadBehaviorBehaviorAllowAndName
    "default" -> pure PBrowserSetDownloadBehaviorBehaviorDefault
    "_" -> fail "failed to parse PBrowserSetDownloadBehaviorBehavior"
instance ToJSON PBrowserSetDownloadBehaviorBehavior where
  toJSON v = A.String $ case v of
    PBrowserSetDownloadBehaviorBehaviorDeny -> "deny"
    PBrowserSetDownloadBehaviorBehaviorAllow -> "allow"
    PBrowserSetDownloadBehaviorBehaviorAllowAndName -> "allowAndName"
    PBrowserSetDownloadBehaviorBehaviorDefault -> "default"
data PBrowserSetDownloadBehavior = PBrowserSetDownloadBehavior
  {
    -- | Whether to allow all or deny all download requests, or use default Chrome behavior if
    --   available (otherwise deny). |allowAndName| allows download and names files according to
    --   their dowmload guids.
    pBrowserSetDownloadBehaviorBehavior :: PBrowserSetDownloadBehaviorBehavior,
    -- | BrowserContext to set download behavior. When omitted, default browser context is used.
    pBrowserSetDownloadBehaviorBrowserContextId :: Maybe BrowserBrowserContextID,
    -- | The default path to save downloaded files to. This is required if behavior is set to 'allow'
    --   or 'allowAndName'.
    pBrowserSetDownloadBehaviorDownloadPath :: Maybe String,
    -- | Whether to emit download events (defaults to false).
    pBrowserSetDownloadBehaviorEventsEnabled :: Maybe Bool
  }
  deriving (Eq, Show)
pBrowserSetDownloadBehavior
  -- | Whether to allow all or deny all download requests, or use default Chrome behavior if
  --   available (otherwise deny). |allowAndName| allows download and names files according to
  --   their dowmload guids.
  :: PBrowserSetDownloadBehaviorBehavior
  -> PBrowserSetDownloadBehavior
pBrowserSetDownloadBehavior
  arg_pBrowserSetDownloadBehaviorBehavior
  = PBrowserSetDownloadBehavior
    arg_pBrowserSetDownloadBehaviorBehavior
    Nothing
    Nothing
    Nothing
instance ToJSON PBrowserSetDownloadBehavior where
  toJSON p = A.object $ catMaybes [
    ("behavior" A..=) <$> Just (pBrowserSetDownloadBehaviorBehavior p),
    ("browserContextId" A..=) <$> (pBrowserSetDownloadBehaviorBrowserContextId p),
    ("downloadPath" A..=) <$> (pBrowserSetDownloadBehaviorDownloadPath p),
    ("eventsEnabled" A..=) <$> (pBrowserSetDownloadBehaviorEventsEnabled p)
    ]
instance Command PBrowserSetDownloadBehavior where
  type CommandResponse PBrowserSetDownloadBehavior = ()
  commandName _ = "Browser.setDownloadBehavior"
  fromJSON = const . A.Success . const ()

-- | Cancel a download if in progress

-- | Parameters of the 'Browser.cancelDownload' command.
data PBrowserCancelDownload = PBrowserCancelDownload
  {
    -- | Global unique identifier of the download.
    pBrowserCancelDownloadGuid :: String,
    -- | BrowserContext to perform the action in. When omitted, default browser context is used.
    pBrowserCancelDownloadBrowserContextId :: Maybe BrowserBrowserContextID
  }
  deriving (Eq, Show)
pBrowserCancelDownload
  -- | Global unique identifier of the download.
  :: String
  -> PBrowserCancelDownload
pBrowserCancelDownload
  arg_pBrowserCancelDownloadGuid
  = PBrowserCancelDownload
    arg_pBrowserCancelDownloadGuid
    Nothing
instance ToJSON PBrowserCancelDownload where
  toJSON p = A.object $ catMaybes [
    ("guid" A..=) <$> Just (pBrowserCancelDownloadGuid p),
    ("browserContextId" A..=) <$> (pBrowserCancelDownloadBrowserContextId p)
    ]
instance Command PBrowserCancelDownload where
  type CommandResponse PBrowserCancelDownload = ()
  commandName _ = "Browser.cancelDownload"
  fromJSON = const . A.Success . const ()

-- | Close browser gracefully.

-- | Parameters of the 'Browser.close' command.
data PBrowserClose = PBrowserClose
  deriving (Eq, Show)
pBrowserClose
  :: PBrowserClose
pBrowserClose
  = PBrowserClose
instance ToJSON PBrowserClose where
  toJSON _ = A.Null
instance Command PBrowserClose where
  type CommandResponse PBrowserClose = ()
  commandName _ = "Browser.close"
  fromJSON = const . A.Success . const ()

-- | Crashes browser on the main thread.

-- | Parameters of the 'Browser.crash' command.
data PBrowserCrash = PBrowserCrash
  deriving (Eq, Show)
pBrowserCrash
  :: PBrowserCrash
pBrowserCrash
  = PBrowserCrash
instance ToJSON PBrowserCrash where
  toJSON _ = A.Null
instance Command PBrowserCrash where
  type CommandResponse PBrowserCrash = ()
  commandName _ = "Browser.crash"
  fromJSON = const . A.Success . const ()

-- | Crashes GPU process.

-- | Parameters of the 'Browser.crashGpuProcess' command.
data PBrowserCrashGpuProcess = PBrowserCrashGpuProcess
  deriving (Eq, Show)
pBrowserCrashGpuProcess
  :: PBrowserCrashGpuProcess
pBrowserCrashGpuProcess
  = PBrowserCrashGpuProcess
instance ToJSON PBrowserCrashGpuProcess where
  toJSON _ = A.Null
instance Command PBrowserCrashGpuProcess where
  type CommandResponse PBrowserCrashGpuProcess = ()
  commandName _ = "Browser.crashGpuProcess"
  fromJSON = const . A.Success . const ()

-- | Returns version information.

-- | Parameters of the 'Browser.getVersion' command.
data PBrowserGetVersion = PBrowserGetVersion
  deriving (Eq, Show)
pBrowserGetVersion
  :: PBrowserGetVersion
pBrowserGetVersion
  = PBrowserGetVersion
instance ToJSON PBrowserGetVersion where
  toJSON _ = A.Null
data BrowserGetVersion = BrowserGetVersion
  {
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
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetVersion where
  parseJSON = A.withObject "BrowserGetVersion" $ \o -> BrowserGetVersion
    <$> o A..: "protocolVersion"
    <*> o A..: "product"
    <*> o A..: "revision"
    <*> o A..: "userAgent"
    <*> o A..: "jsVersion"
instance Command PBrowserGetVersion where
  type CommandResponse PBrowserGetVersion = BrowserGetVersion
  commandName _ = "Browser.getVersion"

-- | Returns the command line switches for the browser process if, and only if
--   --enable-automation is on the commandline.

-- | Parameters of the 'Browser.getBrowserCommandLine' command.
data PBrowserGetBrowserCommandLine = PBrowserGetBrowserCommandLine
  deriving (Eq, Show)
pBrowserGetBrowserCommandLine
  :: PBrowserGetBrowserCommandLine
pBrowserGetBrowserCommandLine
  = PBrowserGetBrowserCommandLine
instance ToJSON PBrowserGetBrowserCommandLine where
  toJSON _ = A.Null
data BrowserGetBrowserCommandLine = BrowserGetBrowserCommandLine
  {
    -- | Commandline parameters
    browserGetBrowserCommandLineArguments :: [String]
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetBrowserCommandLine where
  parseJSON = A.withObject "BrowserGetBrowserCommandLine" $ \o -> BrowserGetBrowserCommandLine
    <$> o A..: "arguments"
instance Command PBrowserGetBrowserCommandLine where
  type CommandResponse PBrowserGetBrowserCommandLine = BrowserGetBrowserCommandLine
  commandName _ = "Browser.getBrowserCommandLine"

-- | Get Chrome histograms.

-- | Parameters of the 'Browser.getHistograms' command.
data PBrowserGetHistograms = PBrowserGetHistograms
  {
    -- | Requested substring in name. Only histograms which have query as a
    --   substring in their name are extracted. An empty or absent query returns
    --   all histograms.
    pBrowserGetHistogramsQuery :: Maybe String,
    -- | If true, retrieve delta since last call.
    pBrowserGetHistogramsDelta :: Maybe Bool
  }
  deriving (Eq, Show)
pBrowserGetHistograms
  :: PBrowserGetHistograms
pBrowserGetHistograms
  = PBrowserGetHistograms
    Nothing
    Nothing
instance ToJSON PBrowserGetHistograms where
  toJSON p = A.object $ catMaybes [
    ("query" A..=) <$> (pBrowserGetHistogramsQuery p),
    ("delta" A..=) <$> (pBrowserGetHistogramsDelta p)
    ]
data BrowserGetHistograms = BrowserGetHistograms
  {
    -- | Histograms.
    browserGetHistogramsHistograms :: [BrowserHistogram]
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetHistograms where
  parseJSON = A.withObject "BrowserGetHistograms" $ \o -> BrowserGetHistograms
    <$> o A..: "histograms"
instance Command PBrowserGetHistograms where
  type CommandResponse PBrowserGetHistograms = BrowserGetHistograms
  commandName _ = "Browser.getHistograms"

-- | Get a Chrome histogram by name.

-- | Parameters of the 'Browser.getHistogram' command.
data PBrowserGetHistogram = PBrowserGetHistogram
  {
    -- | Requested histogram name.
    pBrowserGetHistogramName :: String,
    -- | If true, retrieve delta since last call.
    pBrowserGetHistogramDelta :: Maybe Bool
  }
  deriving (Eq, Show)
pBrowserGetHistogram
  -- | Requested histogram name.
  :: String
  -> PBrowserGetHistogram
pBrowserGetHistogram
  arg_pBrowserGetHistogramName
  = PBrowserGetHistogram
    arg_pBrowserGetHistogramName
    Nothing
instance ToJSON PBrowserGetHistogram where
  toJSON p = A.object $ catMaybes [
    ("name" A..=) <$> Just (pBrowserGetHistogramName p),
    ("delta" A..=) <$> (pBrowserGetHistogramDelta p)
    ]
data BrowserGetHistogram = BrowserGetHistogram
  {
    -- | Histogram.
    browserGetHistogramHistogram :: BrowserHistogram
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetHistogram where
  parseJSON = A.withObject "BrowserGetHistogram" $ \o -> BrowserGetHistogram
    <$> o A..: "histogram"
instance Command PBrowserGetHistogram where
  type CommandResponse PBrowserGetHistogram = BrowserGetHistogram
  commandName _ = "Browser.getHistogram"

-- | Get position and size of the browser window.

-- | Parameters of the 'Browser.getWindowBounds' command.
data PBrowserGetWindowBounds = PBrowserGetWindowBounds
  {
    -- | Browser window id.
    pBrowserGetWindowBoundsWindowId :: BrowserWindowID
  }
  deriving (Eq, Show)
pBrowserGetWindowBounds
  -- | Browser window id.
  :: BrowserWindowID
  -> PBrowserGetWindowBounds
pBrowserGetWindowBounds
  arg_pBrowserGetWindowBoundsWindowId
  = PBrowserGetWindowBounds
    arg_pBrowserGetWindowBoundsWindowId
instance ToJSON PBrowserGetWindowBounds where
  toJSON p = A.object $ catMaybes [
    ("windowId" A..=) <$> Just (pBrowserGetWindowBoundsWindowId p)
    ]
data BrowserGetWindowBounds = BrowserGetWindowBounds
  {
    -- | Bounds information of the window. When window state is 'minimized', the restored window
    --   position and size are returned.
    browserGetWindowBoundsBounds :: BrowserBounds
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetWindowBounds where
  parseJSON = A.withObject "BrowserGetWindowBounds" $ \o -> BrowserGetWindowBounds
    <$> o A..: "bounds"
instance Command PBrowserGetWindowBounds where
  type CommandResponse PBrowserGetWindowBounds = BrowserGetWindowBounds
  commandName _ = "Browser.getWindowBounds"

-- | Get the browser window that contains the devtools target.

-- | Parameters of the 'Browser.getWindowForTarget' command.
data PBrowserGetWindowForTarget = PBrowserGetWindowForTarget
  {
    -- | Devtools agent host id. If called as a part of the session, associated targetId is used.
    pBrowserGetWindowForTargetTargetId :: Maybe TargetTargetID
  }
  deriving (Eq, Show)
pBrowserGetWindowForTarget
  :: PBrowserGetWindowForTarget
pBrowserGetWindowForTarget
  = PBrowserGetWindowForTarget
    Nothing
instance ToJSON PBrowserGetWindowForTarget where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> (pBrowserGetWindowForTargetTargetId p)
    ]
data BrowserGetWindowForTarget = BrowserGetWindowForTarget
  {
    -- | Browser window id.
    browserGetWindowForTargetWindowId :: BrowserWindowID,
    -- | Bounds information of the window. When window state is 'minimized', the restored window
    --   position and size are returned.
    browserGetWindowForTargetBounds :: BrowserBounds
  }
  deriving (Eq, Show)
instance FromJSON BrowserGetWindowForTarget where
  parseJSON = A.withObject "BrowserGetWindowForTarget" $ \o -> BrowserGetWindowForTarget
    <$> o A..: "windowId"
    <*> o A..: "bounds"
instance Command PBrowserGetWindowForTarget where
  type CommandResponse PBrowserGetWindowForTarget = BrowserGetWindowForTarget
  commandName _ = "Browser.getWindowForTarget"

-- | Set position and/or size of the browser window.

-- | Parameters of the 'Browser.setWindowBounds' command.
data PBrowserSetWindowBounds = PBrowserSetWindowBounds
  {
    -- | Browser window id.
    pBrowserSetWindowBoundsWindowId :: BrowserWindowID,
    -- | New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined
    --   with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.
    pBrowserSetWindowBoundsBounds :: BrowserBounds
  }
  deriving (Eq, Show)
pBrowserSetWindowBounds
  -- | Browser window id.
  :: BrowserWindowID
  -- | New window bounds. The 'minimized', 'maximized' and 'fullscreen' states cannot be combined
  --   with 'left', 'top', 'width' or 'height'. Leaves unspecified fields unchanged.
  -> BrowserBounds
  -> PBrowserSetWindowBounds
pBrowserSetWindowBounds
  arg_pBrowserSetWindowBoundsWindowId
  arg_pBrowserSetWindowBoundsBounds
  = PBrowserSetWindowBounds
    arg_pBrowserSetWindowBoundsWindowId
    arg_pBrowserSetWindowBoundsBounds
instance ToJSON PBrowserSetWindowBounds where
  toJSON p = A.object $ catMaybes [
    ("windowId" A..=) <$> Just (pBrowserSetWindowBoundsWindowId p),
    ("bounds" A..=) <$> Just (pBrowserSetWindowBoundsBounds p)
    ]
instance Command PBrowserSetWindowBounds where
  type CommandResponse PBrowserSetWindowBounds = ()
  commandName _ = "Browser.setWindowBounds"
  fromJSON = const . A.Success . const ()

-- | Set dock tile details, platform-specific.

-- | Parameters of the 'Browser.setDockTile' command.
data PBrowserSetDockTile = PBrowserSetDockTile
  {
    pBrowserSetDockTileBadgeLabel :: Maybe String,
    -- | Png encoded image. (Encoded as a base64 string when passed over JSON)
    pBrowserSetDockTileImage :: Maybe String
  }
  deriving (Eq, Show)
pBrowserSetDockTile
  :: PBrowserSetDockTile
pBrowserSetDockTile
  = PBrowserSetDockTile
    Nothing
    Nothing
instance ToJSON PBrowserSetDockTile where
  toJSON p = A.object $ catMaybes [
    ("badgeLabel" A..=) <$> (pBrowserSetDockTileBadgeLabel p),
    ("image" A..=) <$> (pBrowserSetDockTileImage p)
    ]
instance Command PBrowserSetDockTile where
  type CommandResponse PBrowserSetDockTile = ()
  commandName _ = "Browser.setDockTile"
  fromJSON = const . A.Success . const ()

-- | Invoke custom browser commands used by telemetry.

-- | Parameters of the 'Browser.executeBrowserCommand' command.
data PBrowserExecuteBrowserCommand = PBrowserExecuteBrowserCommand
  {
    pBrowserExecuteBrowserCommandCommandId :: BrowserBrowserCommandId
  }
  deriving (Eq, Show)
pBrowserExecuteBrowserCommand
  :: BrowserBrowserCommandId
  -> PBrowserExecuteBrowserCommand
pBrowserExecuteBrowserCommand
  arg_pBrowserExecuteBrowserCommandCommandId
  = PBrowserExecuteBrowserCommand
    arg_pBrowserExecuteBrowserCommandCommandId
instance ToJSON PBrowserExecuteBrowserCommand where
  toJSON p = A.object $ catMaybes [
    ("commandId" A..=) <$> Just (pBrowserExecuteBrowserCommandCommandId p)
    ]
instance Command PBrowserExecuteBrowserCommand where
  type CommandResponse PBrowserExecuteBrowserCommand = ()
  commandName _ = "Browser.executeBrowserCommand"
  fromJSON = const . A.Success . const ()

-- | Type 'Target.TargetID'.
type TargetTargetID = String

-- | Type 'Target.SessionID'.
--   Unique identifier of attached debugging session.
type TargetSessionID = String

-- | Type 'Target.TargetInfo'.
data TargetTargetInfo = TargetTargetInfo
  {
    targetTargetInfoTargetId :: TargetTargetID,
    targetTargetInfoType :: String,
    targetTargetInfoTitle :: String,
    targetTargetInfoUrl :: String,
    -- | Whether the target has an attached client.
    targetTargetInfoAttached :: Bool,
    -- | Opener target Id
    targetTargetInfoOpenerId :: Maybe TargetTargetID,
    -- | Whether the target has access to the originating window.
    targetTargetInfoCanAccessOpener :: Bool,
    -- | Frame id of originating window (is only set if target has an opener).
    targetTargetInfoOpenerFrameId :: Maybe DOMPageNetworkEmulationSecurity.PageFrameId,
    targetTargetInfoBrowserContextId :: Maybe BrowserBrowserContextID,
    -- | Provides additional details for specific target types. For example, for
    --   the type of "page", this may be set to "portal" or "prerender".
    targetTargetInfoSubtype :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON TargetTargetInfo where
  parseJSON = A.withObject "TargetTargetInfo" $ \o -> TargetTargetInfo
    <$> o A..: "targetId"
    <*> o A..: "type"
    <*> o A..: "title"
    <*> o A..: "url"
    <*> o A..: "attached"
    <*> o A..:? "openerId"
    <*> o A..: "canAccessOpener"
    <*> o A..:? "openerFrameId"
    <*> o A..:? "browserContextId"
    <*> o A..:? "subtype"
instance ToJSON TargetTargetInfo where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (targetTargetInfoTargetId p),
    ("type" A..=) <$> Just (targetTargetInfoType p),
    ("title" A..=) <$> Just (targetTargetInfoTitle p),
    ("url" A..=) <$> Just (targetTargetInfoUrl p),
    ("attached" A..=) <$> Just (targetTargetInfoAttached p),
    ("openerId" A..=) <$> (targetTargetInfoOpenerId p),
    ("canAccessOpener" A..=) <$> Just (targetTargetInfoCanAccessOpener p),
    ("openerFrameId" A..=) <$> (targetTargetInfoOpenerFrameId p),
    ("browserContextId" A..=) <$> (targetTargetInfoBrowserContextId p),
    ("subtype" A..=) <$> (targetTargetInfoSubtype p)
    ]

-- | Type 'Target.FilterEntry'.
--   A filter used by target query/discovery/auto-attach operations.
data TargetFilterEntry = TargetFilterEntry
  {
    -- | If set, causes exclusion of mathcing targets from the list.
    targetFilterEntryExclude :: Maybe Bool,
    -- | If not present, matches any type.
    targetFilterEntryType :: Maybe String
  }
  deriving (Eq, Show)
instance FromJSON TargetFilterEntry where
  parseJSON = A.withObject "TargetFilterEntry" $ \o -> TargetFilterEntry
    <$> o A..:? "exclude"
    <*> o A..:? "type"
instance ToJSON TargetFilterEntry where
  toJSON p = A.object $ catMaybes [
    ("exclude" A..=) <$> (targetFilterEntryExclude p),
    ("type" A..=) <$> (targetFilterEntryType p)
    ]

-- | Type 'Target.TargetFilter'.
--   The entries in TargetFilter are matched sequentially against targets and
--   the first entry that matches determines if the target is included or not,
--   depending on the value of `exclude` field in the entry.
--   If filter is not specified, the one assumed is
--   [{type: "browser", exclude: true}, {type: "tab", exclude: true}, {}]
--   (i.e. include everything but `browser` and `tab`).
type TargetTargetFilter = [TargetFilterEntry]

-- | Type 'Target.RemoteLocation'.
data TargetRemoteLocation = TargetRemoteLocation
  {
    targetRemoteLocationHost :: String,
    targetRemoteLocationPort :: Int
  }
  deriving (Eq, Show)
instance FromJSON TargetRemoteLocation where
  parseJSON = A.withObject "TargetRemoteLocation" $ \o -> TargetRemoteLocation
    <$> o A..: "host"
    <*> o A..: "port"
instance ToJSON TargetRemoteLocation where
  toJSON p = A.object $ catMaybes [
    ("host" A..=) <$> Just (targetRemoteLocationHost p),
    ("port" A..=) <$> Just (targetRemoteLocationPort p)
    ]

-- | Type of the 'Target.attachedToTarget' event.
data TargetAttachedToTarget = TargetAttachedToTarget
  {
    -- | Identifier assigned to the session used to send/receive messages.
    targetAttachedToTargetSessionId :: TargetSessionID,
    targetAttachedToTargetTargetInfo :: TargetTargetInfo,
    targetAttachedToTargetWaitingForDebugger :: Bool
  }
  deriving (Eq, Show)
instance FromJSON TargetAttachedToTarget where
  parseJSON = A.withObject "TargetAttachedToTarget" $ \o -> TargetAttachedToTarget
    <$> o A..: "sessionId"
    <*> o A..: "targetInfo"
    <*> o A..: "waitingForDebugger"
instance Event TargetAttachedToTarget where
  eventName _ = "Target.attachedToTarget"

-- | Type of the 'Target.detachedFromTarget' event.
data TargetDetachedFromTarget = TargetDetachedFromTarget
  {
    -- | Detached session identifier.
    targetDetachedFromTargetSessionId :: TargetSessionID
  }
  deriving (Eq, Show)
instance FromJSON TargetDetachedFromTarget where
  parseJSON = A.withObject "TargetDetachedFromTarget" $ \o -> TargetDetachedFromTarget
    <$> o A..: "sessionId"
instance Event TargetDetachedFromTarget where
  eventName _ = "Target.detachedFromTarget"

-- | Type of the 'Target.receivedMessageFromTarget' event.
data TargetReceivedMessageFromTarget = TargetReceivedMessageFromTarget
  {
    -- | Identifier of a session which sends a message.
    targetReceivedMessageFromTargetSessionId :: TargetSessionID,
    targetReceivedMessageFromTargetMessage :: String
  }
  deriving (Eq, Show)
instance FromJSON TargetReceivedMessageFromTarget where
  parseJSON = A.withObject "TargetReceivedMessageFromTarget" $ \o -> TargetReceivedMessageFromTarget
    <$> o A..: "sessionId"
    <*> o A..: "message"
instance Event TargetReceivedMessageFromTarget where
  eventName _ = "Target.receivedMessageFromTarget"

-- | Type of the 'Target.targetCreated' event.
data TargetTargetCreated = TargetTargetCreated
  {
    targetTargetCreatedTargetInfo :: TargetTargetInfo
  }
  deriving (Eq, Show)
instance FromJSON TargetTargetCreated where
  parseJSON = A.withObject "TargetTargetCreated" $ \o -> TargetTargetCreated
    <$> o A..: "targetInfo"
instance Event TargetTargetCreated where
  eventName _ = "Target.targetCreated"

-- | Type of the 'Target.targetDestroyed' event.
data TargetTargetDestroyed = TargetTargetDestroyed
  {
    targetTargetDestroyedTargetId :: TargetTargetID
  }
  deriving (Eq, Show)
instance FromJSON TargetTargetDestroyed where
  parseJSON = A.withObject "TargetTargetDestroyed" $ \o -> TargetTargetDestroyed
    <$> o A..: "targetId"
instance Event TargetTargetDestroyed where
  eventName _ = "Target.targetDestroyed"

-- | Type of the 'Target.targetCrashed' event.
data TargetTargetCrashed = TargetTargetCrashed
  {
    targetTargetCrashedTargetId :: TargetTargetID,
    -- | Termination status type.
    targetTargetCrashedStatus :: String,
    -- | Termination error code.
    targetTargetCrashedErrorCode :: Int
  }
  deriving (Eq, Show)
instance FromJSON TargetTargetCrashed where
  parseJSON = A.withObject "TargetTargetCrashed" $ \o -> TargetTargetCrashed
    <$> o A..: "targetId"
    <*> o A..: "status"
    <*> o A..: "errorCode"
instance Event TargetTargetCrashed where
  eventName _ = "Target.targetCrashed"

-- | Type of the 'Target.targetInfoChanged' event.
data TargetTargetInfoChanged = TargetTargetInfoChanged
  {
    targetTargetInfoChangedTargetInfo :: TargetTargetInfo
  }
  deriving (Eq, Show)
instance FromJSON TargetTargetInfoChanged where
  parseJSON = A.withObject "TargetTargetInfoChanged" $ \o -> TargetTargetInfoChanged
    <$> o A..: "targetInfo"
instance Event TargetTargetInfoChanged where
  eventName _ = "Target.targetInfoChanged"

-- | Activates (focuses) the target.

-- | Parameters of the 'Target.activateTarget' command.
data PTargetActivateTarget = PTargetActivateTarget
  {
    pTargetActivateTargetTargetId :: TargetTargetID
  }
  deriving (Eq, Show)
pTargetActivateTarget
  :: TargetTargetID
  -> PTargetActivateTarget
pTargetActivateTarget
  arg_pTargetActivateTargetTargetId
  = PTargetActivateTarget
    arg_pTargetActivateTargetTargetId
instance ToJSON PTargetActivateTarget where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (pTargetActivateTargetTargetId p)
    ]
instance Command PTargetActivateTarget where
  type CommandResponse PTargetActivateTarget = ()
  commandName _ = "Target.activateTarget"
  fromJSON = const . A.Success . const ()

-- | Attaches to the target with given id.

-- | Parameters of the 'Target.attachToTarget' command.
data PTargetAttachToTarget = PTargetAttachToTarget
  {
    pTargetAttachToTargetTargetId :: TargetTargetID,
    -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
    --   We plan to make this the default, deprecate non-flattened mode,
    --   and eventually retire it. See crbug.com/991325.
    pTargetAttachToTargetFlatten :: Maybe Bool
  }
  deriving (Eq, Show)
pTargetAttachToTarget
  :: TargetTargetID
  -> PTargetAttachToTarget
pTargetAttachToTarget
  arg_pTargetAttachToTargetTargetId
  = PTargetAttachToTarget
    arg_pTargetAttachToTargetTargetId
    Nothing
instance ToJSON PTargetAttachToTarget where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (pTargetAttachToTargetTargetId p),
    ("flatten" A..=) <$> (pTargetAttachToTargetFlatten p)
    ]
data TargetAttachToTarget = TargetAttachToTarget
  {
    -- | Id assigned to the session.
    targetAttachToTargetSessionId :: TargetSessionID
  }
  deriving (Eq, Show)
instance FromJSON TargetAttachToTarget where
  parseJSON = A.withObject "TargetAttachToTarget" $ \o -> TargetAttachToTarget
    <$> o A..: "sessionId"
instance Command PTargetAttachToTarget where
  type CommandResponse PTargetAttachToTarget = TargetAttachToTarget
  commandName _ = "Target.attachToTarget"

-- | Attaches to the browser target, only uses flat sessionId mode.

-- | Parameters of the 'Target.attachToBrowserTarget' command.
data PTargetAttachToBrowserTarget = PTargetAttachToBrowserTarget
  deriving (Eq, Show)
pTargetAttachToBrowserTarget
  :: PTargetAttachToBrowserTarget
pTargetAttachToBrowserTarget
  = PTargetAttachToBrowserTarget
instance ToJSON PTargetAttachToBrowserTarget where
  toJSON _ = A.Null
data TargetAttachToBrowserTarget = TargetAttachToBrowserTarget
  {
    -- | Id assigned to the session.
    targetAttachToBrowserTargetSessionId :: TargetSessionID
  }
  deriving (Eq, Show)
instance FromJSON TargetAttachToBrowserTarget where
  parseJSON = A.withObject "TargetAttachToBrowserTarget" $ \o -> TargetAttachToBrowserTarget
    <$> o A..: "sessionId"
instance Command PTargetAttachToBrowserTarget where
  type CommandResponse PTargetAttachToBrowserTarget = TargetAttachToBrowserTarget
  commandName _ = "Target.attachToBrowserTarget"

-- | Closes the target. If the target is a page that gets closed too.

-- | Parameters of the 'Target.closeTarget' command.
data PTargetCloseTarget = PTargetCloseTarget
  {
    pTargetCloseTargetTargetId :: TargetTargetID
  }
  deriving (Eq, Show)
pTargetCloseTarget
  :: TargetTargetID
  -> PTargetCloseTarget
pTargetCloseTarget
  arg_pTargetCloseTargetTargetId
  = PTargetCloseTarget
    arg_pTargetCloseTargetTargetId
instance ToJSON PTargetCloseTarget where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (pTargetCloseTargetTargetId p)
    ]
instance Command PTargetCloseTarget where
  type CommandResponse PTargetCloseTarget = ()
  commandName _ = "Target.closeTarget"
  fromJSON = const . A.Success . const ()

-- | Inject object to the target's main frame that provides a communication
--   channel with browser target.
--   
--   Injected object will be available as `window[bindingName]`.
--   
--   The object has the follwing API:
--   - `binding.send(json)` - a method to send messages over the remote debugging protocol
--   - `binding.onmessage = json => handleMessage(json)` - a callback that will be called for the protocol notifications and command responses.

-- | Parameters of the 'Target.exposeDevToolsProtocol' command.
data PTargetExposeDevToolsProtocol = PTargetExposeDevToolsProtocol
  {
    pTargetExposeDevToolsProtocolTargetId :: TargetTargetID,
    -- | Binding name, 'cdp' if not specified.
    pTargetExposeDevToolsProtocolBindingName :: Maybe String
  }
  deriving (Eq, Show)
pTargetExposeDevToolsProtocol
  :: TargetTargetID
  -> PTargetExposeDevToolsProtocol
pTargetExposeDevToolsProtocol
  arg_pTargetExposeDevToolsProtocolTargetId
  = PTargetExposeDevToolsProtocol
    arg_pTargetExposeDevToolsProtocolTargetId
    Nothing
instance ToJSON PTargetExposeDevToolsProtocol where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (pTargetExposeDevToolsProtocolTargetId p),
    ("bindingName" A..=) <$> (pTargetExposeDevToolsProtocolBindingName p)
    ]
instance Command PTargetExposeDevToolsProtocol where
  type CommandResponse PTargetExposeDevToolsProtocol = ()
  commandName _ = "Target.exposeDevToolsProtocol"
  fromJSON = const . A.Success . const ()

-- | Creates a new empty BrowserContext. Similar to an incognito profile but you can have more than
--   one.

-- | Parameters of the 'Target.createBrowserContext' command.
data PTargetCreateBrowserContext = PTargetCreateBrowserContext
  {
    -- | If specified, disposes this context when debugging session disconnects.
    pTargetCreateBrowserContextDisposeOnDetach :: Maybe Bool,
    -- | Proxy server, similar to the one passed to --proxy-server
    pTargetCreateBrowserContextProxyServer :: Maybe String,
    -- | Proxy bypass list, similar to the one passed to --proxy-bypass-list
    pTargetCreateBrowserContextProxyBypassList :: Maybe String,
    -- | An optional list of origins to grant unlimited cross-origin access to.
    --   Parts of the URL other than those constituting origin are ignored.
    pTargetCreateBrowserContextOriginsWithUniversalNetworkAccess :: Maybe [String]
  }
  deriving (Eq, Show)
pTargetCreateBrowserContext
  :: PTargetCreateBrowserContext
pTargetCreateBrowserContext
  = PTargetCreateBrowserContext
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PTargetCreateBrowserContext where
  toJSON p = A.object $ catMaybes [
    ("disposeOnDetach" A..=) <$> (pTargetCreateBrowserContextDisposeOnDetach p),
    ("proxyServer" A..=) <$> (pTargetCreateBrowserContextProxyServer p),
    ("proxyBypassList" A..=) <$> (pTargetCreateBrowserContextProxyBypassList p),
    ("originsWithUniversalNetworkAccess" A..=) <$> (pTargetCreateBrowserContextOriginsWithUniversalNetworkAccess p)
    ]
data TargetCreateBrowserContext = TargetCreateBrowserContext
  {
    -- | The id of the context created.
    targetCreateBrowserContextBrowserContextId :: BrowserBrowserContextID
  }
  deriving (Eq, Show)
instance FromJSON TargetCreateBrowserContext where
  parseJSON = A.withObject "TargetCreateBrowserContext" $ \o -> TargetCreateBrowserContext
    <$> o A..: "browserContextId"
instance Command PTargetCreateBrowserContext where
  type CommandResponse PTargetCreateBrowserContext = TargetCreateBrowserContext
  commandName _ = "Target.createBrowserContext"

-- | Returns all browser contexts created with `Target.createBrowserContext` method.

-- | Parameters of the 'Target.getBrowserContexts' command.
data PTargetGetBrowserContexts = PTargetGetBrowserContexts
  deriving (Eq, Show)
pTargetGetBrowserContexts
  :: PTargetGetBrowserContexts
pTargetGetBrowserContexts
  = PTargetGetBrowserContexts
instance ToJSON PTargetGetBrowserContexts where
  toJSON _ = A.Null
data TargetGetBrowserContexts = TargetGetBrowserContexts
  {
    -- | An array of browser context ids.
    targetGetBrowserContextsBrowserContextIds :: [BrowserBrowserContextID]
  }
  deriving (Eq, Show)
instance FromJSON TargetGetBrowserContexts where
  parseJSON = A.withObject "TargetGetBrowserContexts" $ \o -> TargetGetBrowserContexts
    <$> o A..: "browserContextIds"
instance Command PTargetGetBrowserContexts where
  type CommandResponse PTargetGetBrowserContexts = TargetGetBrowserContexts
  commandName _ = "Target.getBrowserContexts"

-- | Creates a new page.

-- | Parameters of the 'Target.createTarget' command.
data PTargetCreateTarget = PTargetCreateTarget
  {
    -- | The initial URL the page will be navigated to. An empty string indicates about:blank.
    pTargetCreateTargetUrl :: String,
    -- | Frame width in DIP (headless chrome only).
    pTargetCreateTargetWidth :: Maybe Int,
    -- | Frame height in DIP (headless chrome only).
    pTargetCreateTargetHeight :: Maybe Int,
    -- | The browser context to create the page in.
    pTargetCreateTargetBrowserContextId :: Maybe BrowserBrowserContextID,
    -- | Whether BeginFrames for this target will be controlled via DevTools (headless chrome only,
    --   not supported on MacOS yet, false by default).
    pTargetCreateTargetEnableBeginFrameControl :: Maybe Bool,
    -- | Whether to create a new Window or Tab (chrome-only, false by default).
    pTargetCreateTargetNewWindow :: Maybe Bool,
    -- | Whether to create the target in background or foreground (chrome-only,
    --   false by default).
    pTargetCreateTargetBackground :: Maybe Bool
  }
  deriving (Eq, Show)
pTargetCreateTarget
  -- | The initial URL the page will be navigated to. An empty string indicates about:blank.
  :: String
  -> PTargetCreateTarget
pTargetCreateTarget
  arg_pTargetCreateTargetUrl
  = PTargetCreateTarget
    arg_pTargetCreateTargetUrl
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
instance ToJSON PTargetCreateTarget where
  toJSON p = A.object $ catMaybes [
    ("url" A..=) <$> Just (pTargetCreateTargetUrl p),
    ("width" A..=) <$> (pTargetCreateTargetWidth p),
    ("height" A..=) <$> (pTargetCreateTargetHeight p),
    ("browserContextId" A..=) <$> (pTargetCreateTargetBrowserContextId p),
    ("enableBeginFrameControl" A..=) <$> (pTargetCreateTargetEnableBeginFrameControl p),
    ("newWindow" A..=) <$> (pTargetCreateTargetNewWindow p),
    ("background" A..=) <$> (pTargetCreateTargetBackground p)
    ]
data TargetCreateTarget = TargetCreateTarget
  {
    -- | The id of the page opened.
    targetCreateTargetTargetId :: TargetTargetID
  }
  deriving (Eq, Show)
instance FromJSON TargetCreateTarget where
  parseJSON = A.withObject "TargetCreateTarget" $ \o -> TargetCreateTarget
    <$> o A..: "targetId"
instance Command PTargetCreateTarget where
  type CommandResponse PTargetCreateTarget = TargetCreateTarget
  commandName _ = "Target.createTarget"

-- | Detaches session with given id.

-- | Parameters of the 'Target.detachFromTarget' command.
data PTargetDetachFromTarget = PTargetDetachFromTarget
  {
    -- | Session to detach.
    pTargetDetachFromTargetSessionId :: Maybe TargetSessionID
  }
  deriving (Eq, Show)
pTargetDetachFromTarget
  :: PTargetDetachFromTarget
pTargetDetachFromTarget
  = PTargetDetachFromTarget
    Nothing
instance ToJSON PTargetDetachFromTarget where
  toJSON p = A.object $ catMaybes [
    ("sessionId" A..=) <$> (pTargetDetachFromTargetSessionId p)
    ]
instance Command PTargetDetachFromTarget where
  type CommandResponse PTargetDetachFromTarget = ()
  commandName _ = "Target.detachFromTarget"
  fromJSON = const . A.Success . const ()

-- | Deletes a BrowserContext. All the belonging pages will be closed without calling their
--   beforeunload hooks.

-- | Parameters of the 'Target.disposeBrowserContext' command.
data PTargetDisposeBrowserContext = PTargetDisposeBrowserContext
  {
    pTargetDisposeBrowserContextBrowserContextId :: BrowserBrowserContextID
  }
  deriving (Eq, Show)
pTargetDisposeBrowserContext
  :: BrowserBrowserContextID
  -> PTargetDisposeBrowserContext
pTargetDisposeBrowserContext
  arg_pTargetDisposeBrowserContextBrowserContextId
  = PTargetDisposeBrowserContext
    arg_pTargetDisposeBrowserContextBrowserContextId
instance ToJSON PTargetDisposeBrowserContext where
  toJSON p = A.object $ catMaybes [
    ("browserContextId" A..=) <$> Just (pTargetDisposeBrowserContextBrowserContextId p)
    ]
instance Command PTargetDisposeBrowserContext where
  type CommandResponse PTargetDisposeBrowserContext = ()
  commandName _ = "Target.disposeBrowserContext"
  fromJSON = const . A.Success . const ()

-- | Returns information about a target.

-- | Parameters of the 'Target.getTargetInfo' command.
data PTargetGetTargetInfo = PTargetGetTargetInfo
  {
    pTargetGetTargetInfoTargetId :: Maybe TargetTargetID
  }
  deriving (Eq, Show)
pTargetGetTargetInfo
  :: PTargetGetTargetInfo
pTargetGetTargetInfo
  = PTargetGetTargetInfo
    Nothing
instance ToJSON PTargetGetTargetInfo where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> (pTargetGetTargetInfoTargetId p)
    ]
data TargetGetTargetInfo = TargetGetTargetInfo
  {
    targetGetTargetInfoTargetInfo :: TargetTargetInfo
  }
  deriving (Eq, Show)
instance FromJSON TargetGetTargetInfo where
  parseJSON = A.withObject "TargetGetTargetInfo" $ \o -> TargetGetTargetInfo
    <$> o A..: "targetInfo"
instance Command PTargetGetTargetInfo where
  type CommandResponse PTargetGetTargetInfo = TargetGetTargetInfo
  commandName _ = "Target.getTargetInfo"

-- | Retrieves a list of available targets.

-- | Parameters of the 'Target.getTargets' command.
data PTargetGetTargets = PTargetGetTargets
  {
    -- | Only targets matching filter will be reported. If filter is not specified
    --   and target discovery is currently enabled, a filter used for target discovery
    --   is used for consistency.
    pTargetGetTargetsFilter :: Maybe TargetTargetFilter
  }
  deriving (Eq, Show)
pTargetGetTargets
  :: PTargetGetTargets
pTargetGetTargets
  = PTargetGetTargets
    Nothing
instance ToJSON PTargetGetTargets where
  toJSON p = A.object $ catMaybes [
    ("filter" A..=) <$> (pTargetGetTargetsFilter p)
    ]
data TargetGetTargets = TargetGetTargets
  {
    -- | The list of targets.
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
  }
  deriving (Eq, Show)
instance FromJSON TargetGetTargets where
  parseJSON = A.withObject "TargetGetTargets" $ \o -> TargetGetTargets
    <$> o A..: "targetInfos"
instance Command PTargetGetTargets where
  type CommandResponse PTargetGetTargets = TargetGetTargets
  commandName _ = "Target.getTargets"

-- | Controls whether to automatically attach to new targets which are considered to be related to
--   this one. When turned on, attaches to all existing related targets as well. When turned off,
--   automatically detaches from all currently attached targets.
--   This also clears all targets added by `autoAttachRelated` from the list of targets to watch
--   for creation of related targets.

-- | Parameters of the 'Target.setAutoAttach' command.
data PTargetSetAutoAttach = PTargetSetAutoAttach
  {
    -- | Whether to auto-attach to related targets.
    pTargetSetAutoAttachAutoAttach :: Bool,
    -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
    --   to run paused targets.
    pTargetSetAutoAttachWaitForDebuggerOnStart :: Bool,
    -- | Enables "flat" access to the session via specifying sessionId attribute in the commands.
    --   We plan to make this the default, deprecate non-flattened mode,
    --   and eventually retire it. See crbug.com/991325.
    pTargetSetAutoAttachFlatten :: Maybe Bool,
    -- | Only targets matching filter will be attached.
    pTargetSetAutoAttachFilter :: Maybe TargetTargetFilter
  }
  deriving (Eq, Show)
pTargetSetAutoAttach
  -- | Whether to auto-attach to related targets.
  :: Bool
  -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
  --   to run paused targets.
  -> Bool
  -> PTargetSetAutoAttach
pTargetSetAutoAttach
  arg_pTargetSetAutoAttachAutoAttach
  arg_pTargetSetAutoAttachWaitForDebuggerOnStart
  = PTargetSetAutoAttach
    arg_pTargetSetAutoAttachAutoAttach
    arg_pTargetSetAutoAttachWaitForDebuggerOnStart
    Nothing
    Nothing
instance ToJSON PTargetSetAutoAttach where
  toJSON p = A.object $ catMaybes [
    ("autoAttach" A..=) <$> Just (pTargetSetAutoAttachAutoAttach p),
    ("waitForDebuggerOnStart" A..=) <$> Just (pTargetSetAutoAttachWaitForDebuggerOnStart p),
    ("flatten" A..=) <$> (pTargetSetAutoAttachFlatten p),
    ("filter" A..=) <$> (pTargetSetAutoAttachFilter p)
    ]
instance Command PTargetSetAutoAttach where
  type CommandResponse PTargetSetAutoAttach = ()
  commandName _ = "Target.setAutoAttach"
  fromJSON = const . A.Success . const ()

-- | Adds the specified target to the list of targets that will be monitored for any related target
--   creation (such as child frames, child workers and new versions of service worker) and reported
--   through `attachedToTarget`. The specified target is also auto-attached.
--   This cancels the effect of any previous `setAutoAttach` and is also cancelled by subsequent
--   `setAutoAttach`. Only available at the Browser target.

-- | Parameters of the 'Target.autoAttachRelated' command.
data PTargetAutoAttachRelated = PTargetAutoAttachRelated
  {
    pTargetAutoAttachRelatedTargetId :: TargetTargetID,
    -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
    --   to run paused targets.
    pTargetAutoAttachRelatedWaitForDebuggerOnStart :: Bool,
    -- | Only targets matching filter will be attached.
    pTargetAutoAttachRelatedFilter :: Maybe TargetTargetFilter
  }
  deriving (Eq, Show)
pTargetAutoAttachRelated
  :: TargetTargetID
  -- | Whether to pause new targets when attaching to them. Use `Runtime.runIfWaitingForDebugger`
  --   to run paused targets.
  -> Bool
  -> PTargetAutoAttachRelated
pTargetAutoAttachRelated
  arg_pTargetAutoAttachRelatedTargetId
  arg_pTargetAutoAttachRelatedWaitForDebuggerOnStart
  = PTargetAutoAttachRelated
    arg_pTargetAutoAttachRelatedTargetId
    arg_pTargetAutoAttachRelatedWaitForDebuggerOnStart
    Nothing
instance ToJSON PTargetAutoAttachRelated where
  toJSON p = A.object $ catMaybes [
    ("targetId" A..=) <$> Just (pTargetAutoAttachRelatedTargetId p),
    ("waitForDebuggerOnStart" A..=) <$> Just (pTargetAutoAttachRelatedWaitForDebuggerOnStart p),
    ("filter" A..=) <$> (pTargetAutoAttachRelatedFilter p)
    ]
instance Command PTargetAutoAttachRelated where
  type CommandResponse PTargetAutoAttachRelated = ()
  commandName _ = "Target.autoAttachRelated"
  fromJSON = const . A.Success . const ()

-- | Controls whether to discover available targets and notify via
--   `targetCreated/targetInfoChanged/targetDestroyed` events.

-- | Parameters of the 'Target.setDiscoverTargets' command.
data PTargetSetDiscoverTargets = PTargetSetDiscoverTargets
  {
    -- | Whether to discover available targets.
    pTargetSetDiscoverTargetsDiscover :: Bool,
    -- | Only targets matching filter will be attached. If `discover` is false,
    --   `filter` must be omitted or empty.
    pTargetSetDiscoverTargetsFilter :: Maybe TargetTargetFilter
  }
  deriving (Eq, Show)
pTargetSetDiscoverTargets
  -- | Whether to discover available targets.
  :: Bool
  -> PTargetSetDiscoverTargets
pTargetSetDiscoverTargets
  arg_pTargetSetDiscoverTargetsDiscover
  = PTargetSetDiscoverTargets
    arg_pTargetSetDiscoverTargetsDiscover
    Nothing
instance ToJSON PTargetSetDiscoverTargets where
  toJSON p = A.object $ catMaybes [
    ("discover" A..=) <$> Just (pTargetSetDiscoverTargetsDiscover p),
    ("filter" A..=) <$> (pTargetSetDiscoverTargetsFilter p)
    ]
instance Command PTargetSetDiscoverTargets where
  type CommandResponse PTargetSetDiscoverTargets = ()
  commandName _ = "Target.setDiscoverTargets"
  fromJSON = const . A.Success . const ()

-- | Enables target discovery for the specified locations, when `setDiscoverTargets` was set to
--   `true`.

-- | Parameters of the 'Target.setRemoteLocations' command.
data PTargetSetRemoteLocations = PTargetSetRemoteLocations
  {
    -- | List of remote locations.
    pTargetSetRemoteLocationsLocations :: [TargetRemoteLocation]
  }
  deriving (Eq, Show)
pTargetSetRemoteLocations
  -- | List of remote locations.
  :: [TargetRemoteLocation]
  -> PTargetSetRemoteLocations
pTargetSetRemoteLocations
  arg_pTargetSetRemoteLocationsLocations
  = PTargetSetRemoteLocations
    arg_pTargetSetRemoteLocationsLocations
instance ToJSON PTargetSetRemoteLocations where
  toJSON p = A.object $ catMaybes [
    ("locations" A..=) <$> Just (pTargetSetRemoteLocationsLocations p)
    ]
instance Command PTargetSetRemoteLocations where
  type CommandResponse PTargetSetRemoteLocations = ()
  commandName _ = "Target.setRemoteLocations"
  fromJSON = const . A.Success . const ()

