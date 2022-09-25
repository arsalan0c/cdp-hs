{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP.Domains.WebAuthn (module CDP.Domains.WebAuthn) where

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



type WebAuthnAuthenticatorId = String
data WebAuthnAuthenticatorProtocol = WebAuthnAuthenticatorProtocolU2f | WebAuthnAuthenticatorProtocolCtap2
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnAuthenticatorProtocol where
   parseJSON = A.withText  "WebAuthnAuthenticatorProtocol"  $ \v -> do
      case v of
         "u2f" -> pure WebAuthnAuthenticatorProtocolU2f
         "ctap2" -> pure WebAuthnAuthenticatorProtocolCtap2
         _ -> fail "failed to parse WebAuthnAuthenticatorProtocol"

instance ToJSON WebAuthnAuthenticatorProtocol where
   toJSON v = A.String $
      case v of
         WebAuthnAuthenticatorProtocolU2f -> "u2f"
         WebAuthnAuthenticatorProtocolCtap2 -> "ctap2"


data WebAuthnCtap2Version = WebAuthnCtap2VersionCtap20 | WebAuthnCtap2VersionCtap21
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnCtap2Version where
   parseJSON = A.withText  "WebAuthnCtap2Version"  $ \v -> do
      case v of
         "ctap2_0" -> pure WebAuthnCtap2VersionCtap20
         "ctap2_1" -> pure WebAuthnCtap2VersionCtap21
         _ -> fail "failed to parse WebAuthnCtap2Version"

instance ToJSON WebAuthnCtap2Version where
   toJSON v = A.String $
      case v of
         WebAuthnCtap2VersionCtap20 -> "ctap2_0"
         WebAuthnCtap2VersionCtap21 -> "ctap2_1"


data WebAuthnAuthenticatorTransport = WebAuthnAuthenticatorTransportUsb | WebAuthnAuthenticatorTransportNfc | WebAuthnAuthenticatorTransportBle | WebAuthnAuthenticatorTransportCable | WebAuthnAuthenticatorTransportInternal
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnAuthenticatorTransport where
   parseJSON = A.withText  "WebAuthnAuthenticatorTransport"  $ \v -> do
      case v of
         "usb" -> pure WebAuthnAuthenticatorTransportUsb
         "nfc" -> pure WebAuthnAuthenticatorTransportNfc
         "ble" -> pure WebAuthnAuthenticatorTransportBle
         "cable" -> pure WebAuthnAuthenticatorTransportCable
         "internal" -> pure WebAuthnAuthenticatorTransportInternal
         _ -> fail "failed to parse WebAuthnAuthenticatorTransport"

instance ToJSON WebAuthnAuthenticatorTransport where
   toJSON v = A.String $
      case v of
         WebAuthnAuthenticatorTransportUsb -> "usb"
         WebAuthnAuthenticatorTransportNfc -> "nfc"
         WebAuthnAuthenticatorTransportBle -> "ble"
         WebAuthnAuthenticatorTransportCable -> "cable"
         WebAuthnAuthenticatorTransportInternal -> "internal"



data WebAuthnVirtualAuthenticatorOptions = WebAuthnVirtualAuthenticatorOptions {
   webAuthnVirtualAuthenticatorOptionsProtocol :: WebAuthnAuthenticatorProtocol,
   webAuthnVirtualAuthenticatorOptionsCtap2Version :: Maybe WebAuthnCtap2Version,
   webAuthnVirtualAuthenticatorOptionsTransport :: WebAuthnAuthenticatorTransport,
   webAuthnVirtualAuthenticatorOptionsHasResidentKey :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsHasUserVerification :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsHasLargeBlob :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsHasCredBlob :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsHasMinPinLength :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsAutomaticPresenceSimulation :: Maybe Bool,
   webAuthnVirtualAuthenticatorOptionsIsUserVerified :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnVirtualAuthenticatorOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  WebAuthnVirtualAuthenticatorOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



data WebAuthnCredential = WebAuthnCredential {
   webAuthnCredentialCredentialId :: String,
   webAuthnCredentialIsResidentCredential :: Bool,
   webAuthnCredentialRpId :: Maybe String,
   webAuthnCredentialPrivateKey :: String,
   webAuthnCredentialUserHandle :: Maybe String,
   webAuthnCredentialSignCount :: Int,
   webAuthnCredentialLargeBlob :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  WebAuthnCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }







data PWebAuthnEnable = PWebAuthnEnable {
   pWebAuthnEnableEnableUi :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


webAuthnEnable :: Handle ev -> PWebAuthnEnable -> IO (Maybe Error)
webAuthnEnable handle params = sendReceiveCommand handle "WebAuthn.enable" (Just params)


webAuthnDisable :: Handle ev -> IO (Maybe Error)
webAuthnDisable handle = sendReceiveCommand handle "WebAuthn.disable" (Nothing :: Maybe ())



data PWebAuthnAddVirtualAuthenticator = PWebAuthnAddVirtualAuthenticator {
   pWebAuthnAddVirtualAuthenticatorOptions :: WebAuthnVirtualAuthenticatorOptions
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


webAuthnAddVirtualAuthenticator :: Handle ev -> PWebAuthnAddVirtualAuthenticator -> IO (Either Error WebAuthnAddVirtualAuthenticator)
webAuthnAddVirtualAuthenticator handle params = sendReceiveCommandResult handle "WebAuthn.addVirtualAuthenticator" (Just params)

data WebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator {
   webAuthnAddVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command WebAuthnAddVirtualAuthenticator where
   commandName _ = "WebAuthn.addVirtualAuthenticator"




data PWebAuthnRemoveVirtualAuthenticator = PWebAuthnRemoveVirtualAuthenticator {
   pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


webAuthnRemoveVirtualAuthenticator :: Handle ev -> PWebAuthnRemoveVirtualAuthenticator -> IO (Maybe Error)
webAuthnRemoveVirtualAuthenticator handle params = sendReceiveCommand handle "WebAuthn.removeVirtualAuthenticator" (Just params)



data PWebAuthnAddCredential = PWebAuthnAddCredential {
   pWebAuthnAddCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
   pWebAuthnAddCredentialCredential :: WebAuthnCredential
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


webAuthnAddCredential :: Handle ev -> PWebAuthnAddCredential -> IO (Maybe Error)
webAuthnAddCredential handle params = sendReceiveCommand handle "WebAuthn.addCredential" (Just params)



data PWebAuthnGetCredential = PWebAuthnGetCredential {
   pWebAuthnGetCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
   pWebAuthnGetCredentialCredentialId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


webAuthnGetCredential :: Handle ev -> PWebAuthnGetCredential -> IO (Either Error WebAuthnGetCredential)
webAuthnGetCredential handle params = sendReceiveCommandResult handle "WebAuthn.getCredential" (Just params)

data WebAuthnGetCredential = WebAuthnGetCredential {
   webAuthnGetCredentialCredential :: WebAuthnCredential
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command WebAuthnGetCredential where
   commandName _ = "WebAuthn.getCredential"




data PWebAuthnGetCredentials = PWebAuthnGetCredentials {
   pWebAuthnGetCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


webAuthnGetCredentials :: Handle ev -> PWebAuthnGetCredentials -> IO (Either Error WebAuthnGetCredentials)
webAuthnGetCredentials handle params = sendReceiveCommandResult handle "WebAuthn.getCredentials" (Just params)

data WebAuthnGetCredentials = WebAuthnGetCredentials {
   webAuthnGetCredentialsCredentials :: [WebAuthnCredential]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command WebAuthnGetCredentials where
   commandName _ = "WebAuthn.getCredentials"




data PWebAuthnRemoveCredential = PWebAuthnRemoveCredential {
   pWebAuthnRemoveCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
   pWebAuthnRemoveCredentialCredentialId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


webAuthnRemoveCredential :: Handle ev -> PWebAuthnRemoveCredential -> IO (Maybe Error)
webAuthnRemoveCredential handle params = sendReceiveCommand handle "WebAuthn.removeCredential" (Just params)



data PWebAuthnClearCredentials = PWebAuthnClearCredentials {
   pWebAuthnClearCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnClearCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnClearCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


webAuthnClearCredentials :: Handle ev -> PWebAuthnClearCredentials -> IO (Maybe Error)
webAuthnClearCredentials handle params = sendReceiveCommand handle "WebAuthn.clearCredentials" (Just params)



data PWebAuthnSetUserVerified = PWebAuthnSetUserVerified {
   pWebAuthnSetUserVerifiedAuthenticatorId :: WebAuthnAuthenticatorId,
   pWebAuthnSetUserVerifiedIsUserVerified :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetUserVerified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetUserVerified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


webAuthnSetUserVerified :: Handle ev -> PWebAuthnSetUserVerified -> IO (Maybe Error)
webAuthnSetUserVerified handle params = sendReceiveCommand handle "WebAuthn.setUserVerified" (Just params)



data PWebAuthnSetAutomaticPresenceSimulation = PWebAuthnSetAutomaticPresenceSimulation {
   pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId :: WebAuthnAuthenticatorId,
   pWebAuthnSetAutomaticPresenceSimulationEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetAutomaticPresenceSimulation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetAutomaticPresenceSimulation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


webAuthnSetAutomaticPresenceSimulation :: Handle ev -> PWebAuthnSetAutomaticPresenceSimulation -> IO (Maybe Error)
webAuthnSetAutomaticPresenceSimulation handle params = sendReceiveCommand handle "WebAuthn.setAutomaticPresenceSimulation" (Just params)



