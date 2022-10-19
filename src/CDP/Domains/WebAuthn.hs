{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
= WebAuthn

This domain allows configuring virtual authenticators to test the WebAuthn
API.
-}


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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Proxy
import System.Random
import GHC.Generics
import Data.Char
import Data.Default

import CDP.Internal.Utils




-- | Type 'WebAuthn.AuthenticatorId'.
type WebAuthnAuthenticatorId = T.Text

-- | Type 'WebAuthn.AuthenticatorProtocol'.
data WebAuthnAuthenticatorProtocol = WebAuthnAuthenticatorProtocolU2f | WebAuthnAuthenticatorProtocolCtap2
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnAuthenticatorProtocol where
  parseJSON = A.withText "WebAuthnAuthenticatorProtocol" $ \v -> case v of
    "u2f" -> pure WebAuthnAuthenticatorProtocolU2f
    "ctap2" -> pure WebAuthnAuthenticatorProtocolCtap2
    "_" -> fail "failed to parse WebAuthnAuthenticatorProtocol"
instance ToJSON WebAuthnAuthenticatorProtocol where
  toJSON v = A.String $ case v of
    WebAuthnAuthenticatorProtocolU2f -> "u2f"
    WebAuthnAuthenticatorProtocolCtap2 -> "ctap2"

-- | Type 'WebAuthn.Ctap2Version'.
data WebAuthnCtap2Version = WebAuthnCtap2VersionCtap2_0 | WebAuthnCtap2VersionCtap2_1
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnCtap2Version where
  parseJSON = A.withText "WebAuthnCtap2Version" $ \v -> case v of
    "ctap2_0" -> pure WebAuthnCtap2VersionCtap2_0
    "ctap2_1" -> pure WebAuthnCtap2VersionCtap2_1
    "_" -> fail "failed to parse WebAuthnCtap2Version"
instance ToJSON WebAuthnCtap2Version where
  toJSON v = A.String $ case v of
    WebAuthnCtap2VersionCtap2_0 -> "ctap2_0"
    WebAuthnCtap2VersionCtap2_1 -> "ctap2_1"

-- | Type 'WebAuthn.AuthenticatorTransport'.
data WebAuthnAuthenticatorTransport = WebAuthnAuthenticatorTransportUsb | WebAuthnAuthenticatorTransportNfc | WebAuthnAuthenticatorTransportBle | WebAuthnAuthenticatorTransportCable | WebAuthnAuthenticatorTransportInternal
  deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnAuthenticatorTransport where
  parseJSON = A.withText "WebAuthnAuthenticatorTransport" $ \v -> case v of
    "usb" -> pure WebAuthnAuthenticatorTransportUsb
    "nfc" -> pure WebAuthnAuthenticatorTransportNfc
    "ble" -> pure WebAuthnAuthenticatorTransportBle
    "cable" -> pure WebAuthnAuthenticatorTransportCable
    "internal" -> pure WebAuthnAuthenticatorTransportInternal
    "_" -> fail "failed to parse WebAuthnAuthenticatorTransport"
instance ToJSON WebAuthnAuthenticatorTransport where
  toJSON v = A.String $ case v of
    WebAuthnAuthenticatorTransportUsb -> "usb"
    WebAuthnAuthenticatorTransportNfc -> "nfc"
    WebAuthnAuthenticatorTransportBle -> "ble"
    WebAuthnAuthenticatorTransportCable -> "cable"
    WebAuthnAuthenticatorTransportInternal -> "internal"

-- | Type 'WebAuthn.VirtualAuthenticatorOptions'.
data WebAuthnVirtualAuthenticatorOptions = WebAuthnVirtualAuthenticatorOptions
  {
    webAuthnVirtualAuthenticatorOptionsProtocol :: WebAuthnAuthenticatorProtocol,
    -- | Defaults to ctap2_0. Ignored if |protocol| == u2f.
    webAuthnVirtualAuthenticatorOptionsCtap2Version :: Maybe WebAuthnCtap2Version,
    webAuthnVirtualAuthenticatorOptionsTransport :: WebAuthnAuthenticatorTransport,
    -- | Defaults to false.
    webAuthnVirtualAuthenticatorOptionsHasResidentKey :: Maybe Bool,
    -- | Defaults to false.
    webAuthnVirtualAuthenticatorOptionsHasUserVerification :: Maybe Bool,
    -- | If set to true, the authenticator will support the largeBlob extension.
    --   https://w3c.github.io/webauthn#largeBlob
    --   Defaults to false.
    webAuthnVirtualAuthenticatorOptionsHasLargeBlob :: Maybe Bool,
    -- | If set to true, the authenticator will support the credBlob extension.
    --   https://fidoalliance.org/specs/fido-v2.1-rd-20201208/fido-client-to-authenticator-protocol-v2.1-rd-20201208.html#sctn-credBlob-extension
    --   Defaults to false.
    webAuthnVirtualAuthenticatorOptionsHasCredBlob :: Maybe Bool,
    -- | If set to true, the authenticator will support the minPinLength extension.
    --   https://fidoalliance.org/specs/fido-v2.1-ps-20210615/fido-client-to-authenticator-protocol-v2.1-ps-20210615.html#sctn-minpinlength-extension
    --   Defaults to false.
    webAuthnVirtualAuthenticatorOptionsHasMinPinLength :: Maybe Bool,
    -- | If set to true, tests of user presence will succeed immediately.
    --   Otherwise, they will not be resolved. Defaults to true.
    webAuthnVirtualAuthenticatorOptionsAutomaticPresenceSimulation :: Maybe Bool,
    -- | Sets whether User Verification succeeds or fails for an authenticator.
    --   Defaults to false.
    webAuthnVirtualAuthenticatorOptionsIsUserVerified :: Maybe Bool
  }
  deriving (Eq, Show)
instance FromJSON WebAuthnVirtualAuthenticatorOptions where
  parseJSON = A.withObject "WebAuthnVirtualAuthenticatorOptions" $ \o -> WebAuthnVirtualAuthenticatorOptions
    <$> o A..: "protocol"
    <*> o A..:? "ctap2Version"
    <*> o A..: "transport"
    <*> o A..:? "hasResidentKey"
    <*> o A..:? "hasUserVerification"
    <*> o A..:? "hasLargeBlob"
    <*> o A..:? "hasCredBlob"
    <*> o A..:? "hasMinPinLength"
    <*> o A..:? "automaticPresenceSimulation"
    <*> o A..:? "isUserVerified"
instance ToJSON WebAuthnVirtualAuthenticatorOptions where
  toJSON p = A.object $ catMaybes [
    ("protocol" A..=) <$> Just (webAuthnVirtualAuthenticatorOptionsProtocol p),
    ("ctap2Version" A..=) <$> (webAuthnVirtualAuthenticatorOptionsCtap2Version p),
    ("transport" A..=) <$> Just (webAuthnVirtualAuthenticatorOptionsTransport p),
    ("hasResidentKey" A..=) <$> (webAuthnVirtualAuthenticatorOptionsHasResidentKey p),
    ("hasUserVerification" A..=) <$> (webAuthnVirtualAuthenticatorOptionsHasUserVerification p),
    ("hasLargeBlob" A..=) <$> (webAuthnVirtualAuthenticatorOptionsHasLargeBlob p),
    ("hasCredBlob" A..=) <$> (webAuthnVirtualAuthenticatorOptionsHasCredBlob p),
    ("hasMinPinLength" A..=) <$> (webAuthnVirtualAuthenticatorOptionsHasMinPinLength p),
    ("automaticPresenceSimulation" A..=) <$> (webAuthnVirtualAuthenticatorOptionsAutomaticPresenceSimulation p),
    ("isUserVerified" A..=) <$> (webAuthnVirtualAuthenticatorOptionsIsUserVerified p)
    ]

-- | Type 'WebAuthn.Credential'.
data WebAuthnCredential = WebAuthnCredential
  {
    webAuthnCredentialCredentialId :: T.Text,
    webAuthnCredentialIsResidentCredential :: Bool,
    -- | Relying Party ID the credential is scoped to. Must be set when adding a
    --   credential.
    webAuthnCredentialRpId :: Maybe T.Text,
    -- | The ECDSA P-256 private key in PKCS#8 format. (Encoded as a base64 string when passed over JSON)
    webAuthnCredentialPrivateKey :: T.Text,
    -- | An opaque byte sequence with a maximum size of 64 bytes mapping the
    --   credential to a specific user. (Encoded as a base64 string when passed over JSON)
    webAuthnCredentialUserHandle :: Maybe T.Text,
    -- | Signature counter. This is incremented by one for each successful
    --   assertion.
    --   See https://w3c.github.io/webauthn/#signature-counter
    webAuthnCredentialSignCount :: Int,
    -- | The large blob associated with the credential.
    --   See https://w3c.github.io/webauthn/#sctn-large-blob-extension (Encoded as a base64 string when passed over JSON)
    webAuthnCredentialLargeBlob :: Maybe T.Text
  }
  deriving (Eq, Show)
instance FromJSON WebAuthnCredential where
  parseJSON = A.withObject "WebAuthnCredential" $ \o -> WebAuthnCredential
    <$> o A..: "credentialId"
    <*> o A..: "isResidentCredential"
    <*> o A..:? "rpId"
    <*> o A..: "privateKey"
    <*> o A..:? "userHandle"
    <*> o A..: "signCount"
    <*> o A..:? "largeBlob"
instance ToJSON WebAuthnCredential where
  toJSON p = A.object $ catMaybes [
    ("credentialId" A..=) <$> Just (webAuthnCredentialCredentialId p),
    ("isResidentCredential" A..=) <$> Just (webAuthnCredentialIsResidentCredential p),
    ("rpId" A..=) <$> (webAuthnCredentialRpId p),
    ("privateKey" A..=) <$> Just (webAuthnCredentialPrivateKey p),
    ("userHandle" A..=) <$> (webAuthnCredentialUserHandle p),
    ("signCount" A..=) <$> Just (webAuthnCredentialSignCount p),
    ("largeBlob" A..=) <$> (webAuthnCredentialLargeBlob p)
    ]

-- | Enable the WebAuthn domain and start intercepting credential storage and
--   retrieval with a virtual authenticator.

-- | Parameters of the 'WebAuthn.enable' command.
data PWebAuthnEnable = PWebAuthnEnable
  {
    -- | Whether to enable the WebAuthn user interface. Enabling the UI is
    --   recommended for debugging and demo purposes, as it is closer to the real
    --   experience. Disabling the UI is recommended for automated testing.
    --   Supported at the embedder's discretion if UI is available.
    --   Defaults to false.
    pWebAuthnEnableEnableUI :: Maybe Bool
  }
  deriving (Eq, Show)
pWebAuthnEnable
  :: PWebAuthnEnable
pWebAuthnEnable
  = PWebAuthnEnable
    Nothing
instance ToJSON PWebAuthnEnable where
  toJSON p = A.object $ catMaybes [
    ("enableUI" A..=) <$> (pWebAuthnEnableEnableUI p)
    ]
instance Command PWebAuthnEnable where
  type CommandResponse PWebAuthnEnable = ()
  commandName _ = "WebAuthn.enable"
  fromJSON = const . A.Success . const ()

-- | Disable the WebAuthn domain.

-- | Parameters of the 'WebAuthn.disable' command.
data PWebAuthnDisable = PWebAuthnDisable
  deriving (Eq, Show)
pWebAuthnDisable
  :: PWebAuthnDisable
pWebAuthnDisable
  = PWebAuthnDisable
instance ToJSON PWebAuthnDisable where
  toJSON _ = A.Null
instance Command PWebAuthnDisable where
  type CommandResponse PWebAuthnDisable = ()
  commandName _ = "WebAuthn.disable"
  fromJSON = const . A.Success . const ()

-- | Creates and adds a virtual authenticator.

-- | Parameters of the 'WebAuthn.addVirtualAuthenticator' command.
data PWebAuthnAddVirtualAuthenticator = PWebAuthnAddVirtualAuthenticator
  {
    pWebAuthnAddVirtualAuthenticatorOptions :: WebAuthnVirtualAuthenticatorOptions
  }
  deriving (Eq, Show)
pWebAuthnAddVirtualAuthenticator
  :: WebAuthnVirtualAuthenticatorOptions
  -> PWebAuthnAddVirtualAuthenticator
pWebAuthnAddVirtualAuthenticator
  arg_pWebAuthnAddVirtualAuthenticatorOptions
  = PWebAuthnAddVirtualAuthenticator
    arg_pWebAuthnAddVirtualAuthenticatorOptions
instance ToJSON PWebAuthnAddVirtualAuthenticator where
  toJSON p = A.object $ catMaybes [
    ("options" A..=) <$> Just (pWebAuthnAddVirtualAuthenticatorOptions p)
    ]
data WebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator
  {
    webAuthnAddVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
  }
  deriving (Eq, Show)
instance FromJSON WebAuthnAddVirtualAuthenticator where
  parseJSON = A.withObject "WebAuthnAddVirtualAuthenticator" $ \o -> WebAuthnAddVirtualAuthenticator
    <$> o A..: "authenticatorId"
instance Command PWebAuthnAddVirtualAuthenticator where
  type CommandResponse PWebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator
  commandName _ = "WebAuthn.addVirtualAuthenticator"

-- | Removes the given authenticator.

-- | Parameters of the 'WebAuthn.removeVirtualAuthenticator' command.
data PWebAuthnRemoveVirtualAuthenticator = PWebAuthnRemoveVirtualAuthenticator
  {
    pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
  }
  deriving (Eq, Show)
pWebAuthnRemoveVirtualAuthenticator
  :: WebAuthnAuthenticatorId
  -> PWebAuthnRemoveVirtualAuthenticator
pWebAuthnRemoveVirtualAuthenticator
  arg_pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId
  = PWebAuthnRemoveVirtualAuthenticator
    arg_pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId
instance ToJSON PWebAuthnRemoveVirtualAuthenticator where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId p)
    ]
instance Command PWebAuthnRemoveVirtualAuthenticator where
  type CommandResponse PWebAuthnRemoveVirtualAuthenticator = ()
  commandName _ = "WebAuthn.removeVirtualAuthenticator"
  fromJSON = const . A.Success . const ()

-- | Adds the credential to the specified authenticator.

-- | Parameters of the 'WebAuthn.addCredential' command.
data PWebAuthnAddCredential = PWebAuthnAddCredential
  {
    pWebAuthnAddCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
    pWebAuthnAddCredentialCredential :: WebAuthnCredential
  }
  deriving (Eq, Show)
pWebAuthnAddCredential
  :: WebAuthnAuthenticatorId
  -> WebAuthnCredential
  -> PWebAuthnAddCredential
pWebAuthnAddCredential
  arg_pWebAuthnAddCredentialAuthenticatorId
  arg_pWebAuthnAddCredentialCredential
  = PWebAuthnAddCredential
    arg_pWebAuthnAddCredentialAuthenticatorId
    arg_pWebAuthnAddCredentialCredential
instance ToJSON PWebAuthnAddCredential where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnAddCredentialAuthenticatorId p),
    ("credential" A..=) <$> Just (pWebAuthnAddCredentialCredential p)
    ]
instance Command PWebAuthnAddCredential where
  type CommandResponse PWebAuthnAddCredential = ()
  commandName _ = "WebAuthn.addCredential"
  fromJSON = const . A.Success . const ()

-- | Returns a single credential stored in the given virtual authenticator that
--   matches the credential ID.

-- | Parameters of the 'WebAuthn.getCredential' command.
data PWebAuthnGetCredential = PWebAuthnGetCredential
  {
    pWebAuthnGetCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
    pWebAuthnGetCredentialCredentialId :: T.Text
  }
  deriving (Eq, Show)
pWebAuthnGetCredential
  :: WebAuthnAuthenticatorId
  -> T.Text
  -> PWebAuthnGetCredential
pWebAuthnGetCredential
  arg_pWebAuthnGetCredentialAuthenticatorId
  arg_pWebAuthnGetCredentialCredentialId
  = PWebAuthnGetCredential
    arg_pWebAuthnGetCredentialAuthenticatorId
    arg_pWebAuthnGetCredentialCredentialId
instance ToJSON PWebAuthnGetCredential where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnGetCredentialAuthenticatorId p),
    ("credentialId" A..=) <$> Just (pWebAuthnGetCredentialCredentialId p)
    ]
data WebAuthnGetCredential = WebAuthnGetCredential
  {
    webAuthnGetCredentialCredential :: WebAuthnCredential
  }
  deriving (Eq, Show)
instance FromJSON WebAuthnGetCredential where
  parseJSON = A.withObject "WebAuthnGetCredential" $ \o -> WebAuthnGetCredential
    <$> o A..: "credential"
instance Command PWebAuthnGetCredential where
  type CommandResponse PWebAuthnGetCredential = WebAuthnGetCredential
  commandName _ = "WebAuthn.getCredential"

-- | Returns all the credentials stored in the given virtual authenticator.

-- | Parameters of the 'WebAuthn.getCredentials' command.
data PWebAuthnGetCredentials = PWebAuthnGetCredentials
  {
    pWebAuthnGetCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
  }
  deriving (Eq, Show)
pWebAuthnGetCredentials
  :: WebAuthnAuthenticatorId
  -> PWebAuthnGetCredentials
pWebAuthnGetCredentials
  arg_pWebAuthnGetCredentialsAuthenticatorId
  = PWebAuthnGetCredentials
    arg_pWebAuthnGetCredentialsAuthenticatorId
instance ToJSON PWebAuthnGetCredentials where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnGetCredentialsAuthenticatorId p)
    ]
data WebAuthnGetCredentials = WebAuthnGetCredentials
  {
    webAuthnGetCredentialsCredentials :: [WebAuthnCredential]
  }
  deriving (Eq, Show)
instance FromJSON WebAuthnGetCredentials where
  parseJSON = A.withObject "WebAuthnGetCredentials" $ \o -> WebAuthnGetCredentials
    <$> o A..: "credentials"
instance Command PWebAuthnGetCredentials where
  type CommandResponse PWebAuthnGetCredentials = WebAuthnGetCredentials
  commandName _ = "WebAuthn.getCredentials"

-- | Removes a credential from the authenticator.

-- | Parameters of the 'WebAuthn.removeCredential' command.
data PWebAuthnRemoveCredential = PWebAuthnRemoveCredential
  {
    pWebAuthnRemoveCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
    pWebAuthnRemoveCredentialCredentialId :: T.Text
  }
  deriving (Eq, Show)
pWebAuthnRemoveCredential
  :: WebAuthnAuthenticatorId
  -> T.Text
  -> PWebAuthnRemoveCredential
pWebAuthnRemoveCredential
  arg_pWebAuthnRemoveCredentialAuthenticatorId
  arg_pWebAuthnRemoveCredentialCredentialId
  = PWebAuthnRemoveCredential
    arg_pWebAuthnRemoveCredentialAuthenticatorId
    arg_pWebAuthnRemoveCredentialCredentialId
instance ToJSON PWebAuthnRemoveCredential where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnRemoveCredentialAuthenticatorId p),
    ("credentialId" A..=) <$> Just (pWebAuthnRemoveCredentialCredentialId p)
    ]
instance Command PWebAuthnRemoveCredential where
  type CommandResponse PWebAuthnRemoveCredential = ()
  commandName _ = "WebAuthn.removeCredential"
  fromJSON = const . A.Success . const ()

-- | Clears all the credentials from the specified device.

-- | Parameters of the 'WebAuthn.clearCredentials' command.
data PWebAuthnClearCredentials = PWebAuthnClearCredentials
  {
    pWebAuthnClearCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
  }
  deriving (Eq, Show)
pWebAuthnClearCredentials
  :: WebAuthnAuthenticatorId
  -> PWebAuthnClearCredentials
pWebAuthnClearCredentials
  arg_pWebAuthnClearCredentialsAuthenticatorId
  = PWebAuthnClearCredentials
    arg_pWebAuthnClearCredentialsAuthenticatorId
instance ToJSON PWebAuthnClearCredentials where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnClearCredentialsAuthenticatorId p)
    ]
instance Command PWebAuthnClearCredentials where
  type CommandResponse PWebAuthnClearCredentials = ()
  commandName _ = "WebAuthn.clearCredentials"
  fromJSON = const . A.Success . const ()

-- | Sets whether User Verification succeeds or fails for an authenticator.
--   The default is true.

-- | Parameters of the 'WebAuthn.setUserVerified' command.
data PWebAuthnSetUserVerified = PWebAuthnSetUserVerified
  {
    pWebAuthnSetUserVerifiedAuthenticatorId :: WebAuthnAuthenticatorId,
    pWebAuthnSetUserVerifiedIsUserVerified :: Bool
  }
  deriving (Eq, Show)
pWebAuthnSetUserVerified
  :: WebAuthnAuthenticatorId
  -> Bool
  -> PWebAuthnSetUserVerified
pWebAuthnSetUserVerified
  arg_pWebAuthnSetUserVerifiedAuthenticatorId
  arg_pWebAuthnSetUserVerifiedIsUserVerified
  = PWebAuthnSetUserVerified
    arg_pWebAuthnSetUserVerifiedAuthenticatorId
    arg_pWebAuthnSetUserVerifiedIsUserVerified
instance ToJSON PWebAuthnSetUserVerified where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnSetUserVerifiedAuthenticatorId p),
    ("isUserVerified" A..=) <$> Just (pWebAuthnSetUserVerifiedIsUserVerified p)
    ]
instance Command PWebAuthnSetUserVerified where
  type CommandResponse PWebAuthnSetUserVerified = ()
  commandName _ = "WebAuthn.setUserVerified"
  fromJSON = const . A.Success . const ()

-- | Sets whether tests of user presence will succeed immediately (if true) or fail to resolve (if false) for an authenticator.
--   The default is true.

-- | Parameters of the 'WebAuthn.setAutomaticPresenceSimulation' command.
data PWebAuthnSetAutomaticPresenceSimulation = PWebAuthnSetAutomaticPresenceSimulation
  {
    pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId :: WebAuthnAuthenticatorId,
    pWebAuthnSetAutomaticPresenceSimulationEnabled :: Bool
  }
  deriving (Eq, Show)
pWebAuthnSetAutomaticPresenceSimulation
  :: WebAuthnAuthenticatorId
  -> Bool
  -> PWebAuthnSetAutomaticPresenceSimulation
pWebAuthnSetAutomaticPresenceSimulation
  arg_pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId
  arg_pWebAuthnSetAutomaticPresenceSimulationEnabled
  = PWebAuthnSetAutomaticPresenceSimulation
    arg_pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId
    arg_pWebAuthnSetAutomaticPresenceSimulationEnabled
instance ToJSON PWebAuthnSetAutomaticPresenceSimulation where
  toJSON p = A.object $ catMaybes [
    ("authenticatorId" A..=) <$> Just (pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId p),
    ("enabled" A..=) <$> Just (pWebAuthnSetAutomaticPresenceSimulationEnabled p)
    ]
instance Command PWebAuthnSetAutomaticPresenceSimulation where
  type CommandResponse PWebAuthnSetAutomaticPresenceSimulation = ()
  commandName _ = "WebAuthn.setAutomaticPresenceSimulation"
  fromJSON = const . A.Success . const ()

