{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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



-- | Type 'WebAuthn.AuthenticatorId' .
type WebAuthnAuthenticatorId = String

-- | Type 'WebAuthn.AuthenticatorProtocol' .
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



-- | Type 'WebAuthn.Ctap2Version' .
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



-- | Type 'WebAuthn.AuthenticatorTransport' .
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



-- | Type 'WebAuthn.VirtualAuthenticatorOptions' .
data WebAuthnVirtualAuthenticatorOptions = WebAuthnVirtualAuthenticatorOptions {

   webAuthnVirtualAuthenticatorOptionsCtap2Version :: WebAuthnVirtualAuthenticatorOptionsCtap2Version, -- ^ Defaults to ctap2_0. Ignored if |protocol| == u2f.

   webAuthnVirtualAuthenticatorOptionsHasResidentKey :: WebAuthnVirtualAuthenticatorOptionsHasResidentKey, -- ^ Defaults to false.
   webAuthnVirtualAuthenticatorOptionsHasUserVerification :: WebAuthnVirtualAuthenticatorOptionsHasUserVerification, -- ^ Defaults to false.
   webAuthnVirtualAuthenticatorOptionsHasLargeBlob :: WebAuthnVirtualAuthenticatorOptionsHasLargeBlob, -- ^ If set to true, the authenticator will support the largeBlob extension.
https://w3c.github.io/webauthn#largeBlob
Defaults to false.
   webAuthnVirtualAuthenticatorOptionsHasCredBlob :: WebAuthnVirtualAuthenticatorOptionsHasCredBlob, -- ^ If set to true, the authenticator will support the credBlob extension.
https://fidoalliance.org/specs/fido-v2.1-rd-20201208/fido-client-to-authenticator-protocol-v2.1-rd-20201208.html#sctn-credBlob-extension
Defaults to false.
   webAuthnVirtualAuthenticatorOptionsHasMinPinLength :: WebAuthnVirtualAuthenticatorOptionsHasMinPinLength, -- ^ If set to true, the authenticator will support the minPinLength extension.
https://fidoalliance.org/specs/fido-v2.1-ps-20210615/fido-client-to-authenticator-protocol-v2.1-ps-20210615.html#sctn-minpinlength-extension
Defaults to false.
   webAuthnVirtualAuthenticatorOptionsAutomaticPresenceSimulation :: WebAuthnVirtualAuthenticatorOptionsAutomaticPresenceSimulation, -- ^ If set to true, tests of user presence will succeed immediately.
Otherwise, they will not be resolved. Defaults to true.
   webAuthnVirtualAuthenticatorOptionsIsUserVerified :: WebAuthnVirtualAuthenticatorOptionsIsUserVerified -- ^ Sets whether User Verification succeeds or fails for an authenticator.
Defaults to false.
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnVirtualAuthenticatorOptions  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  WebAuthnVirtualAuthenticatorOptions where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }



-- | Type 'WebAuthn.Credential' .
data WebAuthnCredential = WebAuthnCredential {


   webAuthnCredentialRpId :: WebAuthnCredentialRpId, -- ^ Relying Party ID the credential is scoped to. Must be set when adding a
credential.
   webAuthnCredentialPrivateKey :: WebAuthnCredentialPrivateKey, -- ^ The ECDSA P-256 private key in PKCS#8 format. (Encoded as a base64 string when passed over JSON)
   webAuthnCredentialUserHandle :: WebAuthnCredentialUserHandle, -- ^ An opaque byte sequence with a maximum size of 64 bytes mapping the
credential to a specific user. (Encoded as a base64 string when passed over JSON)
   webAuthnCredentialSignCount :: WebAuthnCredentialSignCount, -- ^ Signature counter. This is incremented by one for each successful
assertion.
See https://w3c.github.io/webauthn/#signature-counter
   webAuthnCredentialLargeBlob :: WebAuthnCredentialLargeBlob -- ^ The large blob associated with the credential.
See https://w3c.github.io/webauthn/#sctn-large-blob-extension (Encoded as a base64 string when passed over JSON)
} deriving (Generic, Eq, Show, Read)
instance ToJSON WebAuthnCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 , A.omitNothingFields = True}

instance FromJSON  WebAuthnCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 18 }







-- | Parameters of the 'webAuthnEnable' command.
data PWebAuthnEnable = PWebAuthnEnable {
   pWebAuthnEnableEnableUi :: PWebAuthnEnableEnableUi -- ^ Whether to enable the WebAuthn user interface. Enabling the UI is
recommended for debugging and demo purposes, as it is closer to the real
experience. Disabling the UI is recommended for automated testing.
Supported at the embedder's discretion if UI is available.
Defaults to false.
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnEnable  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnEnable where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 15 }


-- | Function for the command 'WebAuthn.enable'.
-- Enable the WebAuthn domain and start intercepting credential storage and
-- retrieval with a virtual authenticator.
-- Parameters: 'PWebAuthnEnable'
webAuthnEnable :: Handle ev -> PWebAuthnEnable -> IO (Maybe Error)
webAuthnEnable handle params = sendReceiveCommand handle "WebAuthn.enable" (Just params)


-- | Function for the command 'WebAuthn.disable'.
-- Disable the WebAuthn domain.
webAuthnDisable :: Handle ev -> IO (Maybe Error)
webAuthnDisable handle = sendReceiveCommand handle "WebAuthn.disable" (Nothing :: Maybe ())


-- | Parameters of the 'webAuthnAddVirtualAuthenticator' command.
data PWebAuthnAddVirtualAuthenticator = PWebAuthnAddVirtualAuthenticator {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 32 }


-- | Function for the command 'WebAuthn.addVirtualAuthenticator'.
-- Creates and adds a virtual authenticator.
-- Parameters: 'PWebAuthnAddVirtualAuthenticator'
-- Returns: 'WebAuthnAddVirtualAuthenticator'
webAuthnAddVirtualAuthenticator :: Handle ev -> PWebAuthnAddVirtualAuthenticator -> IO (Either Error WebAuthnAddVirtualAuthenticator)
webAuthnAddVirtualAuthenticator handle params = sendReceiveCommandResult handle "WebAuthn.addVirtualAuthenticator" (Just params)

-- | Return type of the 'webAuthnAddVirtualAuthenticator' command.
data WebAuthnAddVirtualAuthenticator = WebAuthnAddVirtualAuthenticator {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnAddVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 31 }

instance Command WebAuthnAddVirtualAuthenticator where
   commandName _ = "WebAuthn.addVirtualAuthenticator"



-- | Parameters of the 'webAuthnRemoveVirtualAuthenticator' command.
data PWebAuthnRemoveVirtualAuthenticator = PWebAuthnRemoveVirtualAuthenticator {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveVirtualAuthenticator  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveVirtualAuthenticator where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 35 }


-- | Function for the command 'WebAuthn.removeVirtualAuthenticator'.
-- Removes the given authenticator.
-- Parameters: 'PWebAuthnRemoveVirtualAuthenticator'
webAuthnRemoveVirtualAuthenticator :: Handle ev -> PWebAuthnRemoveVirtualAuthenticator -> IO (Maybe Error)
webAuthnRemoveVirtualAuthenticator handle params = sendReceiveCommand handle "WebAuthn.removeVirtualAuthenticator" (Just params)


-- | Parameters of the 'webAuthnAddCredential' command.
data PWebAuthnAddCredential = PWebAuthnAddCredential {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnAddCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnAddCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'WebAuthn.addCredential'.
-- Adds the credential to the specified authenticator.
-- Parameters: 'PWebAuthnAddCredential'
webAuthnAddCredential :: Handle ev -> PWebAuthnAddCredential -> IO (Maybe Error)
webAuthnAddCredential handle params = sendReceiveCommand handle "WebAuthn.addCredential" (Just params)


-- | Parameters of the 'webAuthnGetCredential' command.
data PWebAuthnGetCredential = PWebAuthnGetCredential {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }


-- | Function for the command 'WebAuthn.getCredential'.
-- Returns a single credential stored in the given virtual authenticator that
-- matches the credential ID.
-- Parameters: 'PWebAuthnGetCredential'
-- Returns: 'WebAuthnGetCredential'
webAuthnGetCredential :: Handle ev -> PWebAuthnGetCredential -> IO (Either Error WebAuthnGetCredential)
webAuthnGetCredential handle params = sendReceiveCommandResult handle "WebAuthn.getCredential" (Just params)

-- | Return type of the 'webAuthnGetCredential' command.
data WebAuthnGetCredential = WebAuthnGetCredential {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 21 }

instance Command WebAuthnGetCredential where
   commandName _ = "WebAuthn.getCredential"



-- | Parameters of the 'webAuthnGetCredentials' command.
data PWebAuthnGetCredentials = PWebAuthnGetCredentials {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnGetCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 23 }


-- | Function for the command 'WebAuthn.getCredentials'.
-- Returns all the credentials stored in the given virtual authenticator.
-- Parameters: 'PWebAuthnGetCredentials'
-- Returns: 'WebAuthnGetCredentials'
webAuthnGetCredentials :: Handle ev -> PWebAuthnGetCredentials -> IO (Either Error WebAuthnGetCredentials)
webAuthnGetCredentials handle params = sendReceiveCommandResult handle "WebAuthn.getCredentials" (Just params)

-- | Return type of the 'webAuthnGetCredentials' command.
data WebAuthnGetCredentials = WebAuthnGetCredentials {

} deriving (Generic, Eq, Show, Read)

instance FromJSON  WebAuthnGetCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 22 }

instance Command WebAuthnGetCredentials where
   commandName _ = "WebAuthn.getCredentials"



-- | Parameters of the 'webAuthnRemoveCredential' command.
data PWebAuthnRemoveCredential = PWebAuthnRemoveCredential {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnRemoveCredential  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnRemoveCredential where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'WebAuthn.removeCredential'.
-- Removes a credential from the authenticator.
-- Parameters: 'PWebAuthnRemoveCredential'
webAuthnRemoveCredential :: Handle ev -> PWebAuthnRemoveCredential -> IO (Maybe Error)
webAuthnRemoveCredential handle params = sendReceiveCommand handle "WebAuthn.removeCredential" (Just params)


-- | Parameters of the 'webAuthnClearCredentials' command.
data PWebAuthnClearCredentials = PWebAuthnClearCredentials {
} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnClearCredentials  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnClearCredentials where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 25 }


-- | Function for the command 'WebAuthn.clearCredentials'.
-- Clears all the credentials from the specified device.
-- Parameters: 'PWebAuthnClearCredentials'
webAuthnClearCredentials :: Handle ev -> PWebAuthnClearCredentials -> IO (Maybe Error)
webAuthnClearCredentials handle params = sendReceiveCommand handle "WebAuthn.clearCredentials" (Just params)


-- | Parameters of the 'webAuthnSetUserVerified' command.
data PWebAuthnSetUserVerified = PWebAuthnSetUserVerified {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetUserVerified  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetUserVerified where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 24 }


-- | Function for the command 'WebAuthn.setUserVerified'.
-- Sets whether User Verification succeeds or fails for an authenticator.
-- The default is true.
-- Parameters: 'PWebAuthnSetUserVerified'
webAuthnSetUserVerified :: Handle ev -> PWebAuthnSetUserVerified -> IO (Maybe Error)
webAuthnSetUserVerified handle params = sendReceiveCommand handle "WebAuthn.setUserVerified" (Just params)


-- | Parameters of the 'webAuthnSetAutomaticPresenceSimulation' command.
data PWebAuthnSetAutomaticPresenceSimulation = PWebAuthnSetAutomaticPresenceSimulation {


} deriving (Generic, Eq, Show, Read)
instance ToJSON PWebAuthnSetAutomaticPresenceSimulation  where
   toJSON = A.genericToJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 , A.omitNothingFields = True}

instance FromJSON  PWebAuthnSetAutomaticPresenceSimulation where
   parseJSON = A.genericParseJSON A.defaultOptions{A.fieldLabelModifier = uncapitalizeFirst . drop 39 }


-- | Function for the command 'WebAuthn.setAutomaticPresenceSimulation'.
-- Sets whether tests of user presence will succeed immediately (if true) or fail to resolve (if false) for an authenticator.
-- The default is true.
-- Parameters: 'PWebAuthnSetAutomaticPresenceSimulation'
webAuthnSetAutomaticPresenceSimulation :: Handle ev -> PWebAuthnSetAutomaticPresenceSimulation -> IO (Maybe Error)
webAuthnSetAutomaticPresenceSimulation handle params = sendReceiveCommand handle "WebAuthn.setAutomaticPresenceSimulation" (Just params)



