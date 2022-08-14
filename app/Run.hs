{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}

module Run (module Run) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
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


defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222)

hostPortToEndpoint :: (String, Int) -> Http.Request
hostPortToEndpoint (host, port) = Http.parseRequest_ . 
    ("GET " <>) . 
    mconcat $ [host, ":", show port, "/json"]

type ClientApp a b c = Session a b -> IO c
type Handler a = a -> IO ()

data Session a b = MkSession 
    { events       :: MVar (Map.Map a (Handler b))
    , conn         :: WS.Connection
    , listenThread :: ThreadId
    }

data PageInfo = PageInfo
    { debuggerUrl :: String
    } deriving Show
instance FromJSON PageInfo where
    parseJSON = A.withObject "PageInfo" $ \v ->
        PageInfo <$> v .: "webSocketDebuggerUrl"
    
parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of
            (':':str)   -> read str
            _           -> 80
    pure (Uri.uriRegName auth, port, Uri.uriPath u)

getPageInfo :: Http.Request -> IO PageInfo
getPageInfo request = do
    response <- Http.httpLBS request
    let body = Http.getResponseBody response
    case A.decode body of
        Just mpis -> pure $ head . catMaybes $ mpis
        Nothing   -> error "getPageInfo: Parse error"

runClient :: Maybe (String, Int) -> ClientApp a b c -> IO c
runClient hostPort app = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi   <- getPageInfo endpoint
    eventsM <- newMVar Map.empty
    let (host, port, path) = parseUri . debuggerUrl $ pi
    
    WS.runClient host port path $ \conn -> do
        listenThread <- forkIO $ forever $ do
            res <- WS.fromDataMessage <$> WS.receiveDataMessage conn
            let eventResponse = fromMaybe (error "could not parse event") (A.decode res)
            handler <- withMVar eventsM $ \evs -> do
                let (EventResponse ev) = eventResponse
                pure . fromMaybe (putStrLn . const ("received event: " <> show ev)) . Map.lookup ev $ evs 
            
            let eventResponseResult = fromMaybe (error "could not parse event") (A.decode res)
            (\h -> h . errParams $ eventResponseResult) handler                    
        
        killThread listenThread
        let session = MkSession eventsM conn listenThread
        result <- app session
        -- killThread listenThread
        pure result


updateEvents :: (Map.Map a (Handler b) -> Map.Map a (Handler b)) -> Session a b -> IO ()
updateEvents f = ($ pure . f) . modifyMVar_ . events

eventSubscribe :: (Ord a) => a -> Handler b -> Session a b -> IO ()
eventSubscribe ev newHandler session = updateEvents
    (Map.alter (const . Just $ newHandler) ev)
    session

eventUnsubscribe :: (Ord a) => a -> Session a b -> IO ()
eventUnsubscribe ev = updateEvents (Map.delete ev)

errParams :: EventResponseResult -> b
errParams (EventResponseResult _ params) = params
data EventResponseResult where
    EventResponseResult :: (FromJSON a, Show a, Eq a, Ord a, Read a, FromJSON b) => a -> b -> EventResponseResult

data EventResponse a where
    EventResponse :: (FromJSON a, Show a, Eq a, Ord a, Read a) => a -> EventResponse a

instance (FromJSON a, Show a, Eq a, Ord a, Read a) => FromJSON (EventResponse a) where
    parseJSON = A.withObject "EventResponse" $ \obj -> do
        erEvent <- obj .: "method"
        pure . EventResponse $ erEvent


data ToJSONEx where
    ToJSONEx :: (ToJSON a, Show a) => a -> ToJSONEx
instance ToJSON ToJSONEx where
    toJSON (ToJSONEx v) = toJSON v
instance Show ToJSONEx where
    show (ToJSONEx v) = show v
data Command = Command {
        commandId :: Int
    , commandMethod :: String
    , commandParams :: [(String, ToJSONEx)]
    } deriving Show
instance ToJSON Command where
    toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= commandParams cmd
        ]
        
methodToName :: T.Text -> String
methodToName md = let [domain, method] = T.splitOn "." md in
    mconcat [T.unpack domain, C.pascal . T.unpack $ method]

data CommandResponseResult a = CommandResponseResult { crrId :: Int, crrResult :: a }
data CommandResponse = CommandResponse { crId :: Int }

instance (Show a) => Show (CommandResponseResult a) where
    show CommandResponseResult{..} = "\nid: " <> show crrId <> "\nresult: " <> show crrResult
instance Show CommandResponse where
    show CommandResponse{..} = "\nid: " <> show crId

instance (FromJSON a) => FromJSON (CommandResponseResult a) where
    parseJSON = A.withObject "CommandResponseResult" $ \obj -> do
        crrId <- obj .: "id"
        crrResult <- obj .: "result"
        pure CommandResponseResult{..}

instance FromJSON CommandResponse where
    parseJSON = A.withObject "CommandResponse" $ \obj -> do
        crId <- obj .: "id"
        pure CommandResponse{..}

newtype Error = Error String
    deriving Show

indent :: Int -> String -> String
indent = (<>) . flip replicate ' '

commandToStr :: Command -> String
commandToStr Command{..} = unlines 
    [ "command: " <> commandMethod
    , if (not . null) commandParams 
        then "arguments: " <> (unlines . map (indent 2 . (\(f,s) -> f <> ":" <> s) . fmap show) $ commandParams)
        else ""
    ]

sendCommand :: WS.Connection -> (String, String) -> [(String, ToJSONEx)] -> IO Command
sendCommand conn (domain,method) paramArgs = do
    putStrLn . show $ (domain, method)
    id <- pure 1  -- TODO: randomly generate
    let c = command id
    WS.sendTextData conn . A.encode $ c
    pure c
    where
    command id = Command id 
        (domain <> "." <> method)
        paramArgs
    
receiveResponse :: (FromJSON a) => WS.Connection -> IO (Maybe a)
receiveResponse conn = A.decode <$> do
    dm <- WS.receiveDataMessage conn
    putStrLn . show $ dm
    pure $ WS.fromDataMessage dm

sendReceiveCommandResult :: FromJSON b =>
    WS.Connection ->
            (String, String) -> [(String, ToJSONEx)]
            -> IO (Either Error b)
sendReceiveCommandResult conn (domain,method) paramArgs = do
    command <- sendCommand conn (domain,method) paramArgs
    res     <- receiveResponse conn
    pure $ maybe (Left . responseParseError $ command) 
        (maybe (Left . responseParseError $ command) Right . crrResult) res 

sendReceiveCommand ::
    WS.Connection ->
            (String, String) -> [(String, ToJSONEx)]
            -> IO (Maybe Error)
sendReceiveCommand conn (domain,method) paramArgs = do
    command <- sendCommand conn (domain, method) paramArgs
    res     <- receiveResponse conn
    pure $ maybe (Just . responseParseError $ command) 
        (const Nothing . crId) res

responseParseError :: Command -> Error
responseParseError c = Error . unlines $
    ["unable to parse response", commandToStr c]

data EventResult where
    EventResult :: (FromJSON a, Eq a, Show a, Read a) => a -> EventResult