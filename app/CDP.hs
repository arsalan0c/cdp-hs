{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}

module CDP (module CDP) where

import           Control.Applicative  ((<$>))
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe)
import Data.Functor.Identity
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO         as TI
import qualified Data.Vector          as V
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))
import qualified Data.Aeson           as A
import qualified Network.HTTP.Simple as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets as WS


defaultHostPort :: (String, Int)
defaultHostPort = ("http://127.0.0.1", 9222)

hostPortToEndpoint :: (String, Int) -> Http.Request
hostPortToEndpoint (host, port) = Http.parseRequest_ . 
    ("GET " <>) . 
    mconcat $ [host, ":", show port, "/json"]

type ClientApp a = Session -> IO a
newtype Session = MkSession 
    { conn :: WS.Connection
    }

runClient :: Maybe (String, Int) -> ClientApp a -> IO a
runClient hostPort f = do
    let endpoint = hostPortToEndpoint . fromMaybe defaultHostPort $ hostPort
    pi <- getPageInfo endpoint
    let (host, port, path) = parseUri . debuggerUrl $ pi
    WS.runClient host port path (f . MkSession)

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
    }
instance ToJSON Command where
   toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= (M.fromList $ commandParams cmd)
        ]

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

sendCommand :: WS.Connection -> Command -> IO ()
sendCommand conn command = WS.sendTextData conn . A.encode $ command
   
receiveResponse :: (FromJSON a) => WS.Connection -> IO (Maybe a)
receiveResponse conn = A.decode <$> do
    dm <- WS.receiveDataMessage conn
    pure $ WS.fromDataMessage dm

sendReceiveCommandResult :: FromJSON b =>
    WS.Connection ->
         (String, String) -> [(String, ToJSONEx)]
         -> IO (Either Error b)
sendReceiveCommandResult conn (domain,method) paramArgs = do
    sendCommand conn command
    res <- receiveResponse conn
    pure $ maybe (Left . responseParseError $ command) 
        (maybe (Left . responseParseError $ command) Right . crrResult) res 
  where
    command = Command 1 -- TODO: increment
        (domain <> "." <> method)
        paramArgs

sendReceiveCommand ::
    WS.Connection ->
         (String, String) -> [(String, ToJSONEx)]
         -> IO (Maybe Error)
sendReceiveCommand conn (domain,method) paramArgs = do
    sendCommand conn command
    res <- receiveResponse conn
    pure $ maybe (Just . responseParseError $ command) 
        (const Nothing . crId) res
  where
    command = Command 1 -- TODO: increment
        (domain <> "." <> method)
        paramArgs

responseParseError :: Command -> Error
responseParseError c = Error . unlines $
    ["unable to parse response", commandToStr c]






browserClose :: Session -> IO (Maybe Error)
browserClose session  = sendReceiveCommand (conn session) ("Browser","close") ([] ++ (catMaybes []))

data BrowserGetVersion = BrowserGetVersion {
    browserGetVersionProtocolVersion :: String,
    browserGetVersionProduct :: String,
    browserGetVersionRevision :: String,
    browserGetVersionUserAgent :: String,
    browserGetVersionJsVersion :: String
} deriving Show
instance FromJSON BrowserGetVersion where
    parseJSON = A.withObject "BrowserGetVersion" $ \v -> 
        BrowserGetVersion <$> v .: "protocolVersion"
            <*> v .: "product"
            <*> v .: "revision"
            <*> v .: "userAgent"
            <*> v .: "jsVersion"



browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session  = sendReceiveCommandResult (conn session) ("Browser","getVersion") ([] ++ (catMaybes []))




data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
    pageAddScriptToEvaluateOnNewDocumentIdentifier :: ()
} deriving Show
instance FromJSON PageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "PageAddScriptToEvaluateOnNewDocument" $ \v -> 
        PageAddScriptToEvaluateOnNewDocument <$> v .: "identifier"



pageAddScriptToEvaluateOnNewDocument :: Session -> String -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument session source = sendReceiveCommandResult (conn session) ("Page","addScriptToEvaluateOnNewDocument") ([("source", ToJSONEx source)] ++ (catMaybes []))


pageBringToFront :: Session -> IO (Maybe Error)
pageBringToFront session  = sendReceiveCommand (conn session) ("Page","bringToFront") ([] ++ (catMaybes []))

data PageCaptureScreenshot = PageCaptureScreenshot {
    pageCaptureScreenshotData :: String
} deriving Show
instance FromJSON PageCaptureScreenshot where
    parseJSON = A.withObject "PageCaptureScreenshot" $ \v -> 
        PageCaptureScreenshot <$> v .: "data"



pageCaptureScreenshot :: Session -> Maybe String -> Maybe Int -> Maybe () -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot session format quality clip = sendReceiveCommandResult (conn session) ("Page","captureScreenshot") ([] ++ (catMaybes [fmap (("format",) . ToJSONEx) format, fmap (("quality",) . ToJSONEx) quality, fmap (("clip",) . ToJSONEx) clip]))

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
    pageCreateIsolatedWorldExecutionContextId :: ()
} deriving Show
instance FromJSON PageCreateIsolatedWorld where
    parseJSON = A.withObject "PageCreateIsolatedWorld" $ \v -> 
        PageCreateIsolatedWorld <$> v .: "executionContextId"



pageCreateIsolatedWorld :: Session -> () -> Maybe String -> Maybe Bool -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld session frameId worldName grantUniveralAccess = sendReceiveCommandResult (conn session) ("Page","createIsolatedWorld") ([("frameId", ToJSONEx frameId)] ++ (catMaybes [fmap (("worldName",) . ToJSONEx) worldName, fmap (("grantUniveralAccess",) . ToJSONEx) grantUniveralAccess]))


pageDisable :: Session -> IO (Maybe Error)
pageDisable session  = sendReceiveCommand (conn session) ("Page","disable") ([] ++ (catMaybes []))


pageEnable :: Session -> IO (Maybe Error)
pageEnable session  = sendReceiveCommand (conn session) ("Page","enable") ([] ++ (catMaybes []))

data PageGetAppManifest = PageGetAppManifest {
    pageGetAppManifestUrl :: String,
    pageGetAppManifestErrors :: [String],
    pageGetAppManifestData :: Maybe String
} deriving Show
instance FromJSON PageGetAppManifest where
    parseJSON = A.withObject "PageGetAppManifest" $ \v -> 
        PageGetAppManifest <$> v .: "url"
            <*> v .: "errors"
            <*> v .:? "data"



pageGetAppManifest :: Session -> IO (Either Error PageGetAppManifest)
pageGetAppManifest session  = sendReceiveCommandResult (conn session) ("Page","getAppManifest") ([] ++ (catMaybes []))

data PageGetFrameTree = PageGetFrameTree {
    pageGetFrameTreeFrameTree :: ()
} deriving Show
instance FromJSON PageGetFrameTree where
    parseJSON = A.withObject "PageGetFrameTree" $ \v -> 
        PageGetFrameTree <$> v .: "frameTree"



pageGetFrameTree :: Session -> IO (Either Error PageGetFrameTree)
pageGetFrameTree session  = sendReceiveCommandResult (conn session) ("Page","getFrameTree") ([] ++ (catMaybes []))

data PageGetLayoutMetrics = PageGetLayoutMetrics {
    pageGetLayoutMetricsCssLayoutViewport :: (),
    pageGetLayoutMetricsCssVisualViewport :: (),
    pageGetLayoutMetricsCssContentSize :: ()
} deriving Show
instance FromJSON PageGetLayoutMetrics where
    parseJSON = A.withObject "PageGetLayoutMetrics" $ \v -> 
        PageGetLayoutMetrics <$> v .: "cssLayoutViewport"
            <*> v .: "cssVisualViewport"
            <*> v .: "cssContentSize"



pageGetLayoutMetrics :: Session -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics session  = sendReceiveCommandResult (conn session) ("Page","getLayoutMetrics") ([] ++ (catMaybes []))

data PageGetNavigationHistory = PageGetNavigationHistory {
    pageGetNavigationHistoryCurrentIndex :: Int,
    pageGetNavigationHistoryEntries :: [String]
} deriving Show
instance FromJSON PageGetNavigationHistory where
    parseJSON = A.withObject "PageGetNavigationHistory" $ \v -> 
        PageGetNavigationHistory <$> v .: "currentIndex"
            <*> v .: "entries"



pageGetNavigationHistory :: Session -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory session  = sendReceiveCommandResult (conn session) ("Page","getNavigationHistory") ([] ++ (catMaybes []))


pageResetNavigationHistory :: Session -> IO (Maybe Error)
pageResetNavigationHistory session  = sendReceiveCommand (conn session) ("Page","resetNavigationHistory") ([] ++ (catMaybes []))


pageHandleJavaScriptDialog :: Session -> Bool -> Maybe String -> IO (Maybe Error)
pageHandleJavaScriptDialog session accept promptText = sendReceiveCommand (conn session) ("Page","handleJavaScriptDialog") ([("accept", ToJSONEx accept)] ++ (catMaybes [fmap (("promptText",) . ToJSONEx) promptText]))

data PageNavigate = PageNavigate {
    pageNavigateFrameId :: (),
    pageNavigateLoaderId :: Maybe (),
    pageNavigateErrorText :: Maybe String
} deriving Show
instance FromJSON PageNavigate where
    parseJSON = A.withObject "PageNavigate" $ \v -> 
        PageNavigate <$> v .: "frameId"
            <*> v .:? "loaderId"
            <*> v .:? "errorText"



pageNavigate :: Session -> String -> Maybe String -> Maybe () -> Maybe () -> IO (Either Error PageNavigate)
pageNavigate session url referrer transitionType frameId = sendReceiveCommandResult (conn session) ("Page","navigate") ([("url", ToJSONEx url)] ++ (catMaybes [fmap (("referrer",) . ToJSONEx) referrer, fmap (("transitionType",) . ToJSONEx) transitionType, fmap (("frameId",) . ToJSONEx) frameId]))


pageNavigateToHistoryEntry :: Session -> Int -> IO (Maybe Error)
pageNavigateToHistoryEntry session entryId = sendReceiveCommand (conn session) ("Page","navigateToHistoryEntry") ([("entryId", ToJSONEx entryId)] ++ (catMaybes []))

data PagePrintToPDF = PagePrintToPDF {
    pagePrintToPDFData :: String
} deriving Show
instance FromJSON PagePrintToPDF where
    parseJSON = A.withObject "PagePrintToPDF" $ \v -> 
        PagePrintToPDF <$> v .: "data"



pagePrintToPDF :: Session -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> IO (Either Error PagePrintToPDF)
pagePrintToPDF session landscape displayHeaderFooter printBackground scale paperWidth paperHeight marginTop marginBottom marginLeft marginRight pageRanges headerTemplate footerTemplate preferCSSPageSize = sendReceiveCommandResult (conn session) ("Page","printToPDF") ([] ++ (catMaybes [fmap (("landscape",) . ToJSONEx) landscape, fmap (("displayHeaderFooter",) . ToJSONEx) displayHeaderFooter, fmap (("printBackground",) . ToJSONEx) printBackground, fmap (("scale",) . ToJSONEx) scale, fmap (("paperWidth",) . ToJSONEx) paperWidth, fmap (("paperHeight",) . ToJSONEx) paperHeight, fmap (("marginTop",) . ToJSONEx) marginTop, fmap (("marginBottom",) . ToJSONEx) marginBottom, fmap (("marginLeft",) . ToJSONEx) marginLeft, fmap (("marginRight",) . ToJSONEx) marginRight, fmap (("pageRanges",) . ToJSONEx) pageRanges, fmap (("headerTemplate",) . ToJSONEx) headerTemplate, fmap (("footerTemplate",) . ToJSONEx) footerTemplate, fmap (("preferCSSPageSize",) . ToJSONEx) preferCSSPageSize]))


pageReload :: Session -> Maybe Bool -> Maybe String -> IO (Maybe Error)
pageReload session ignoreCache scriptToEvaluateOnLoad = sendReceiveCommand (conn session) ("Page","reload") ([] ++ (catMaybes [fmap (("ignoreCache",) . ToJSONEx) ignoreCache, fmap (("scriptToEvaluateOnLoad",) . ToJSONEx) scriptToEvaluateOnLoad]))


pageRemoveScriptToEvaluateOnNewDocument :: Session -> () -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument session identifier = sendReceiveCommand (conn session) ("Page","removeScriptToEvaluateOnNewDocument") ([("identifier", ToJSONEx identifier)] ++ (catMaybes []))


pageSetDocumentContent :: Session -> () -> String -> IO (Maybe Error)
pageSetDocumentContent session frameId html = sendReceiveCommand (conn session) ("Page","setDocumentContent") ([("frameId", ToJSONEx frameId), ("html", ToJSONEx html)] ++ (catMaybes []))


pageStopLoading :: Session -> IO (Maybe Error)
pageStopLoading session  = sendReceiveCommand (conn session) ("Page","stopLoading") ([] ++ (catMaybes []))




