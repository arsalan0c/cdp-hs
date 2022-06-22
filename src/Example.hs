{-# LANGUAGE OverloadedStrings #-}
module Example (perform) where

import           Control.Applicative  ((<$>))
import           Control.Monad       
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson           as A
import qualified Network.HTTP.Simple as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets as WS

data PageInfo = PageInfo
    { debuggerUrl :: String
    } deriving Show
instance FromJSON PageInfo where
    parseJSON = A.withObject "PageInfo" $ \v ->
        PageInfo <$> v .: "webSocketDebuggerUrl"

data Command = Command
    { commandId :: Int
    , commandMethod :: String
    , commandParams :: [(String, String)]
    } deriving Show

instance ToJSON Command where
    toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= (M.fromList $ commandParams cmd)
        ]

getPageInfo :: Http.Request -> IO PageInfo
getPageInfo request = do
    response <- Http.httpLBS request
    let body = Http.getResponseBody response
    print body
    case A.decode body of
        Just mpis -> pure $ head . catMaybes $ mpis
        Nothing   -> error "getPageInfo: Parse error"
    
parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of
            (':':str)   -> read str
            _           -> 80

    pure (Uri.uriRegName auth, port, Uri.uriPath u)

sendCommand :: WS.Connection -> (String, String) -> [(String, String)] -> IO ()
sendCommand conn (domain, method) params = WS.sendTextData conn $ 
    A.encode $ 
    Command { commandId = 1
            , commandMethod = domain <> "." <> method
            , commandParams = params
            }

receiveCommandResponse :: (FromJSON a) => WS.Connection -> IO (Maybe (CommandResult a))
receiveCommandResponse conn = A.decode <$> do
    dm <- WS.receiveDataMessage conn
    pure $ WS.fromDataMessage dm
    
sendReceiveCommand :: (FromJSON a) => 
    WS.Connection -> 
        (String, String) -> [(String, String)] 
        -> IO (Maybe (CommandResult a))
sendReceiveCommand conn domain_method params = do 
    sendCommand conn domain_method params
    receiveCommandResponse conn

browserClose :: WS.Connection -> IO ()
browserClose conn = sendCommand conn ("Browser", "close") []

browserGetVersion :: WS.Connection -> IO (Maybe BrowserVersion)
browserGetVersion conn = fmap result <$> sendReceiveCommand conn ("Browser", "getVersion") []

data BrowserVersion = BrowserVersion {
    protocolVersion :: String
,   product :: String
,   revision :: String
,   userAgent :: String
,   jsVersion :: String
} deriving Show

instance FromJSON BrowserVersion where
    parseJSON = A.withObject "BrowserVersion" $ \v ->
        BrowserVersion <$> v .: "protocolVersion"
            <*> v .: "product"
            <*> v .: "revision"
            <*> v .: "userAgent"
            <*> v .: "jsVersion"

newtype ScriptIdentifier = ScriptIdentifier { identifier :: String }
    deriving Show
instance FromJSON ScriptIdentifier where
    parseJSON = A.withObject "ScriptIdentifier" $ \v ->
        ScriptIdentifier <$> v .: "identifier"

addScriptToEvaluateOnNewDocument :: WS.Connection -> String -> IO (Maybe ScriptIdentifier)
addScriptToEvaluateOnNewDocument conn source = fmap result <$> sendReceiveCommand conn ("Page","addScriptToEvaluateOnNewDocument") [("source", source)]

data CommandResult a = CommandResult { id :: Int, result :: a }
instance (FromJSON a) => FromJSON (CommandResult a) where
    parseJSON = A.withObject "CommandResult" $ \v ->
        CommandResult <$> v .: "id" <*> v .: "result"

perform :: IO ()
perform = do
    pi <- getPageInfo "http://127.0.0.1:9222/json"
    putStrLn $ show pi
    let (host, port, path) = parseUri (debuggerUrl pi)
    WS.runClient host port path $ \conn -> do
        bv <- addScriptToEvaluateOnNewDocument conn "console.log(10);"
        print bv
    
        forever $ do
            msg <- WS.receiveData conn
            liftIO $ TI.putStrLn msg
    
