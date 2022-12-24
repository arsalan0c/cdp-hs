{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}

module CDP.Endpoints where

import Data.Maybe
import Data.List
import Data.Proxy
import qualified Network.URI          as Uri
import qualified Network.HTTP.Simple  as Http
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))
import qualified Data.Text as T
import Control.Exception

import CDP.Internal.Utils
import qualified CDP.Definition

type URL      = T.Text
type TargetId = T.Text

data EPBrowserVersion   = EPBrowserVersion
data EPAllTargets       = EPAllTargets
data EPCurrentProtocol  = EPCurrentProtocol
data EPOpenNewTab       = EPOpenNewTab       { unOpenNewTab :: URL }
data EPActivateTarget   = EPActivateTarget   { unActivateTarget :: TargetId }
data EPCloseTarget      = EPCloseTarget      { unCloseTarget :: TargetId }
data EPFrontend         = EPFrontend

data SomeEndpoint where
    SomeEndpoint :: Endpoint ep => ep -> SomeEndpoint

fromSomeEndpoint :: (forall ep. Endpoint ep => ep -> r) -> SomeEndpoint -> r
fromSomeEndpoint f (SomeEndpoint ep) = f ep

-- | Sends a request with the given parameters to the corresponding endpoint
endpoint :: Endpoint ep => Config -> ep -> IO (EndpointResponse ep)
endpoint = getEndpoint . hostPort

-- | Creates a session with a new tab
connectToTab :: Config -> URL -> IO TargetInfo
connectToTab cfg url = do
        targetInfo <- endpoint cfg $ EPOpenNewTab url
        endpoint cfg $ EPActivateTarget $ tiId targetInfo
        pure targetInfo

class Endpoint ep where
    type EndpointResponse ep :: *
    getEndpoint :: (String, Int) -> ep -> IO (EndpointResponse ep)
    epDecode :: Proxy ep -> BS.ByteString -> Either String (EndpointResponse ep)

instance Endpoint EPBrowserVersion where
    type EndpointResponse EPBrowserVersion = BrowserVersion
    getEndpoint hostPort _ = performRequest (Proxy :: Proxy EPBrowserVersion) $
        getRequest hostPort ["json", "version"] Nothing
    epDecode = const A.eitherDecode

instance Endpoint EPAllTargets where
    type EndpointResponse EPAllTargets = [TargetInfo]
    getEndpoint hostPort _ = performRequest (Proxy :: Proxy EPAllTargets) $ 
        getRequest hostPort ["json", "list"] Nothing
    epDecode = const A.eitherDecode

instance Endpoint EPCurrentProtocol where
    type EndpointResponse EPCurrentProtocol = CDP.Definition.TopLevel
    getEndpoint hostPort _ = performRequest (Proxy :: Proxy EPCurrentProtocol) $
        getRequest hostPort ["json", "protocol"] Nothing
    epDecode = const A.eitherDecode

instance Endpoint EPOpenNewTab where
    type EndpointResponse EPOpenNewTab = TargetInfo
    getEndpoint hostPort (EPOpenNewTab url) = performRequest (Proxy :: Proxy EPOpenNewTab) $
        getRequest hostPort ["json", "new"] (Just url)
    epDecode = const A.eitherDecode 

instance Endpoint EPActivateTarget where
    type EndpointResponse EPActivateTarget = ()
    getEndpoint hostPort (EPActivateTarget id) = performRequest (Proxy :: Proxy EPActivateTarget) $
        getRequest hostPort ["json", "activate", id] Nothing
    epDecode = const . const $ Right ()

instance Endpoint EPCloseTarget where
    type EndpointResponse EPCloseTarget = ()
    getEndpoint hostPort (EPCloseTarget id) = performRequest (Proxy :: Proxy EPCloseTarget) $
        getRequest hostPort ["json", "close", id] Nothing
    epDecode = const . const $ Right ()

instance Endpoint EPFrontend where
    type EndpointResponse EPFrontend = BS.ByteString
    getEndpoint hostPort EPFrontend = performRequest (Proxy :: Proxy EPFrontend) $
        getRequest hostPort ["devtools", "inspector.html"] Nothing
    epDecode = const Right

data BrowserVersion = BrowserVersion
    { bvBrowser              :: T.Text
    , bvProtocolVersion      :: T.Text
    , bvUserAgent            :: T.Text
    , bvV8Version            :: T.Text
    , bvVebKitVersion        :: T.Text
    , bvWebSocketDebuggerUrl :: T.Text
    } deriving (Show, Eq)
instance FromJSON BrowserVersion where
    parseJSON = A.withObject "BrowserVersion" $ \v ->
        BrowserVersion <$> v .: "Browser"
            <*> v .: "Protocol-Version"
            <*> v .: "User-Agent"
            <*> v .: "V8-Version"
            <*> v .: "WebKit-Version"
            <*> v .: "webSocketDebuggerUrl"

data TargetInfo = TargetInfo
    { tiDescription          :: T.Text
    , tiDevtoolsFrontendUrl  :: T.Text
    , tiId                   :: T.Text
    , tiTitle                :: T.Text
    , tiType                 :: T.Text
    , tiUrl                  :: T.Text
    , tiWebSocketDebuggerUrl :: T.Text
    } deriving Show
instance FromJSON TargetInfo where
    parseJSON = A.withObject "TargetInfo" $ \v ->
        TargetInfo <$> v .: "description"
            <*> v .: "devtoolsFrontendUrl"
            <*> v .: "id"
            <*> v .: "title"
            <*> v .: "type"
            <*> v .: "url"
            <*> v .: "webSocketDebuggerUrl"

browserAddress :: (String, Int) -> IO (String, Int, String)
browserAddress hostPort = fromMaybe (throw . ERRParse $ "invalid URI when connecting to browser") . 
    parseUri . T.unpack . bvWebSocketDebuggerUrl <$> getEndpoint hostPort EPBrowserVersion

pageAddress :: (String, Int) -> IO (String, Int, String)
pageAddress hostPort = fromMaybe (throw . ERRParse $ "invalid URI when connecting to page") . 
    parseUri . T.unpack . tiWebSocketDebuggerUrl . head <$> getEndpoint hostPort EPAllTargets

getRequest :: (String, Int) -> [T.Text] -> Maybe T.Text -> Http.Request
getRequest (host, port) path mbParam = Http.parseRequest_ . T.unpack $ r
  where
    r = mconcat ["GET ", T.pack host, ":", T.pack (show port), "/", T.intercalate "/" path
                , maybe "" ("?" <>) mbParam 
                ]

performRequest :: Endpoint ep => Proxy ep -> Http.Request -> IO (EndpointResponse ep)
performRequest p req = do
    body <- Http.getResponseBody <$> Http.httpLBS req
    either (throwIO . ERRParse) pure $ epDecode p body

parseUri :: String -> Maybe (String, Int, String)
parseUri uri = do
    u    <- Uri.parseURI $ uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of
            (':':str)   -> read str
            _           -> 80
    pure (Uri.uriRegName auth, port, Uri.uriPath u)