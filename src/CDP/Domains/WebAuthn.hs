{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


{- |
  WebAuthn :
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
type WebAuthnAuthenticatorId = String

-- | Type 'WebAuthn.AuthenticatorProtocol'.
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



-- | Type 'WebAuthn.Ctap2Version'.
data WebAuthnCtap2Version = WebAuthnCtap2VersionCtap2_0 | WebAuthnCtap2VersionCtap2_1
   deriving (Ord, Eq, Show, Read)
instance FromJSON WebAuthnCtap2Version where
   parseJSON = A.withText  "WebAuthnCtap2Version"  $ \v -> do
      case v of
         "ctap2_0" -> pure WebAuthnCtap2VersionCtap2_0
         "ctap2_1" -> pure WebAuthnCtap2VersionCtap2_1
         _ -> fail "failed to parse WebAuthnCtap2Version"

instance ToJSON WebAuthnCtap2Version where
   toJSON v = A.String $
      case v of
         WebAuthnCtap2VersionCtap2_0 -> "ctap2_0"
         WebAuthnCtap2VersionCtap2_1 -> "ctap2_1"



-- | Type 'WebAuthn.AuthenticatorTransport'.
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



-- | Type 'WebAuthn.VirtualAuthenticatorOptions'.
data WebAuthnVirtualAuthenticatorOptions = WebAuthnVirtualAuthenticatorOptions {
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
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnVirtualAuthenticatorOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  WebAuthnVirtualAuthenticatorOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'WebAuthn.Credential'.
data WebAuthnCredential = WebAuthnCredential {
  webAuthnCredentialCredentialId :: String,
  webAuthnCredentialIsResidentCredential :: Bool,
  -- | Relying Party ID the credential is scoped to. Must be set when adding a
  --   credential.
  webAuthnCredentialRpId :: Maybe String,
  -- | The ECDSA P-256 private key in PKCS#8 format. (Encoded as a base64 string when passed over JSON)
  webAuthnCredentialPrivateKey :: String,
  -- | An opaque byte sequence with a maximum size of 64 bytes mapping the
  --   credential to a specific user. (Encoded as a base64 string when passed over JSON)
  webAuthnCredentialUserHandle :: Maybe String,
  -- | Signature counter. This is incremented by one for each successful
  --   assertion.
  --   See https://w3c.github.io/webauthn/#signature-counter
  webAuthnCredentialSignCount :: Int,
  -- | The large blob associated with the credential.
  --   See https://w3c.github.io/webauthn/#sctn-large-blob-extension (Encoded as a base64 string when passed over JSON)
  webAuthnCredentialLargeBlob :: Maybe String
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  WebAuthnCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }







-- | WebAuthn.enable
--   Enable the WebAuthn domain and start intercepting credential storage and
--   retrieval with a virtual authenticator.

-- | Parameters of the 'WebAuthn.enable' command.
data PWebAuthnEnable = PWebAuthnEnable {
  -- | Whether to enable the WebAuthn user interface. Enabling the UI is
  --   recommended for debugging and demo purposes, as it is closer to the real
  --   experience. Disabling the UI is recommended for automated testing.
  --   Supported at the embedder's discretion if UI is available.
  --   Defaults to false.
  pWebAuthnEnableEnableUI :: Maybe Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


instance Command PWebAuthnEnable where
   type CommandResponse PWebAuthnEnable = ()
   commandName _ = "WebAuthn.enable"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.disable
--   Disable the WebAuthn domain.

-- | Parameters of the 'WebAuthn.disable' command.
data PWebAuthnDisable = PWebAuthnDisable
instance ToJSON PWebAuthnDisable where toJSON _ = A.Null

instance Command PWebAuthnDisable where
   type CommandResponse PWebAuthnDisable = ()
   commandName _ = "WebAuthn.disable"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.addVirtualAuthenticator
--   Creates and adds a virtual authenticator.

-- | Parameters of the 'WebAuthn.addVirtualAuthenticator' command.
data PWebAuthnAddVirtualAuthenticator = PWebAuthnAddVirtualAuthenticator {
  pWebAuthnAddVirtualAuthenticatorOptions :: WebAuthnVirtualAuthenticatorOptions
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Return type of the 'WebAuthn.addVirtualAuthenticator' command.
data WebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator {
  webAuthnAddVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command PWebAuthnAddVirtualAuthenticator where
   type CommandResponse PWebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator
   commandName _ = "WebAuthn.addVirtualAuthenticator"



-- | WebAuthn.removeVirtualAuthenticator
--   Removes the given authenticator.

-- | Parameters of the 'WebAuthn.removeVirtualAuthenticator' command.
data PWebAuthnRemoveVirtualAuthenticator = PWebAuthnRemoveVirtualAuthenticator {
  pWebAuthnRemoveVirtualAuthenticatorAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


instance Command PWebAuthnRemoveVirtualAuthenticator where
   type CommandResponse PWebAuthnRemoveVirtualAuthenticator = ()
   commandName _ = "WebAuthn.removeVirtualAuthenticator"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.addCredential
--   Adds the credential to the specified authenticator.

-- | Parameters of the 'WebAuthn.addCredential' command.
data PWebAuthnAddCredential = PWebAuthnAddCredential {
  pWebAuthnAddCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
  pWebAuthnAddCredentialCredential :: WebAuthnCredential
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


instance Command PWebAuthnAddCredential where
   type CommandResponse PWebAuthnAddCredential = ()
   commandName _ = "WebAuthn.addCredential"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.getCredential
--   Returns a single credential stored in the given virtual authenticator that
--   matches the credential ID.

-- | Parameters of the 'WebAuthn.getCredential' command.
data PWebAuthnGetCredential = PWebAuthnGetCredential {
  pWebAuthnGetCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
  pWebAuthnGetCredentialCredentialId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Return type of the 'WebAuthn.getCredential' command.
data WebAuthnGetCredential = WebAuthnGetCredential {
  webAuthnGetCredentialCredential :: WebAuthnCredential
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command PWebAuthnGetCredential where
   type CommandResponse PWebAuthnGetCredential = WebAuthnGetCredential
   commandName _ = "WebAuthn.getCredential"



-- | WebAuthn.getCredentials
--   Returns all the credentials stored in the given virtual authenticator.

-- | Parameters of the 'WebAuthn.getCredentials' command.
data PWebAuthnGetCredentials = PWebAuthnGetCredentials {
  pWebAuthnGetCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Return type of the 'WebAuthn.getCredentials' command.
data WebAuthnGetCredentials = WebAuthnGetCredentials {
  webAuthnGetCredentialsCredentials :: [WebAuthnCredential]
} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command PWebAuthnGetCredentials where
   type CommandResponse PWebAuthnGetCredentials = WebAuthnGetCredentials
   commandName _ = "WebAuthn.getCredentials"



-- | WebAuthn.removeCredential
--   Removes a credential from the authenticator.

-- | Parameters of the 'WebAuthn.removeCredential' command.
data PWebAuthnRemoveCredential = PWebAuthnRemoveCredential {
  pWebAuthnRemoveCredentialAuthenticatorId :: WebAuthnAuthenticatorId,
  pWebAuthnRemoveCredentialCredentialId :: String
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PWebAuthnRemoveCredential where
   type CommandResponse PWebAuthnRemoveCredential = ()
   commandName _ = "WebAuthn.removeCredential"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.clearCredentials
--   Clears all the credentials from the specified device.

-- | Parameters of the 'WebAuthn.clearCredentials' command.
data PWebAuthnClearCredentials = PWebAuthnClearCredentials {
  pWebAuthnClearCredentialsAuthenticatorId :: WebAuthnAuthenticatorId
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnClearCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnClearCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


instance Command PWebAuthnClearCredentials where
   type CommandResponse PWebAuthnClearCredentials = ()
   commandName _ = "WebAuthn.clearCredentials"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.setUserVerified
--   Sets whether User Verification succeeds or fails for an authenticator.
--   The default is true.

-- | Parameters of the 'WebAuthn.setUserVerified' command.
data PWebAuthnSetUserVerified = PWebAuthnSetUserVerified {
  pWebAuthnSetUserVerifiedAuthenticatorId :: WebAuthnAuthenticatorId,
  pWebAuthnSetUserVerifiedIsUserVerified :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetUserVerified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetUserVerified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


instance Command PWebAuthnSetUserVerified where
   type CommandResponse PWebAuthnSetUserVerified = ()
   commandName _ = "WebAuthn.setUserVerified"
   fromJSON = const . A.Success . const ()


-- | WebAuthn.setAutomaticPresenceSimulation
--   Sets whether tests of user presence will succeed immediately (if true) or fail to resolve (if false) for an authenticator.
--   The default is true.

-- | Parameters of the 'WebAuthn.setAutomaticPresenceSimulation' command.
data PWebAuthnSetAutomaticPresenceSimulation = PWebAuthnSetAutomaticPresenceSimulation {
  pWebAuthnSetAutomaticPresenceSimulationAuthenticatorId :: WebAuthnAuthenticatorId,
  pWebAuthnSetAutomaticPresenceSimulationEnabled :: Bool
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetAutomaticPresenceSimulation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetAutomaticPresenceSimulation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


instance Command PWebAuthnSetAutomaticPresenceSimulation where
   type CommandResponse PWebAuthnSetAutomaticPresenceSimulation = ()
   commandName _ = "WebAuthn.setAutomaticPresenceSimulation"
   fromJSON = const . A.Success . const ()



