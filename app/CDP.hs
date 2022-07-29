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
instance FromJSON  BrowserGetVersion where
    parseJSON = A.withObject "BrowserGetVersion" $ \v ->
         BrowserGetVersion <$> v .:  "protocolVersion"
            <*> v  .:  "product"
            <*> v  .:  "revision"
            <*> v  .:  "userAgent"
            <*> v  .:  "jsVersion"



browserGetVersion :: Session -> IO (Either Error BrowserGetVersion)
browserGetVersion session  = sendReceiveCommandResult (conn session) ("Browser","getVersion") ([] ++ (catMaybes []))



type DOMNodeId = Int

type DOMBackendNodeId = Int

data DOMBackendNode = DOMBackendNode {
    domBackendNodeNodeType :: Int,
    domBackendNodeNodeName :: String,
    domBackendNodeBackendNodeId :: DOMBackendNodeId
} deriving Show
instance FromJSON  DOMBackendNode where
    parseJSON = A.withObject "DOMBackendNode" $ \v ->
         DOMBackendNode <$> v .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "backendNodeId"


instance ToJSON DOMBackendNode  where
    toJSON v = A.object
        [ "nodeType" .= domBackendNodeNodeType v
        , "nodeName" .= domBackendNodeNodeName v
        , "backendNodeId" .= domBackendNodeBackendNodeId v
        ]



data DOMPseudoType = DOMPseudoTypeFirstLine | DOMPseudoTypeFirstLetter | DOMPseudoTypeBefore | DOMPseudoTypeAfter | DOMPseudoTypeMarker | DOMPseudoTypeBackdrop | DOMPseudoTypeSelection | DOMPseudoTypeTargetText | DOMPseudoTypeSpellingError | DOMPseudoTypeGrammarError | DOMPseudoTypeHighlight | DOMPseudoTypeFirstLineInherited | DOMPseudoTypeScrollbar | DOMPseudoTypeScrollbarThumb | DOMPseudoTypeScrollbarButton | DOMPseudoTypeScrollbarTrack | DOMPseudoTypeScrollbarTrackPiece | DOMPseudoTypeScrollbarCorner | DOMPseudoTypeResizer | DOMPseudoTypeInputListButton | DOMPseudoTypePageTransition | DOMPseudoTypePageTransitionContainer | DOMPseudoTypePageTransitionImageWrapper | DOMPseudoTypePageTransitionOutgoingImage | DOMPseudoTypePageTransitionIncomingImage
    deriving Show
instance FromJSON DOMPseudoType where
    parseJSON = A.withText  "DOMPseudoType"  $ \v -> 
        pure $ case v of
                "first-line" -> DOMPseudoTypeFirstLine
                "first-letter" -> DOMPseudoTypeFirstLetter
                "before" -> DOMPseudoTypeBefore
                "after" -> DOMPseudoTypeAfter
                "marker" -> DOMPseudoTypeMarker
                "backdrop" -> DOMPseudoTypeBackdrop
                "selection" -> DOMPseudoTypeSelection
                "target-text" -> DOMPseudoTypeTargetText
                "spelling-error" -> DOMPseudoTypeSpellingError
                "grammar-error" -> DOMPseudoTypeGrammarError
                "highlight" -> DOMPseudoTypeHighlight
                "first-line-inherited" -> DOMPseudoTypeFirstLineInherited
                "scrollbar" -> DOMPseudoTypeScrollbar
                "scrollbar-thumb" -> DOMPseudoTypeScrollbarThumb
                "scrollbar-button" -> DOMPseudoTypeScrollbarButton
                "scrollbar-track" -> DOMPseudoTypeScrollbarTrack
                "scrollbar-track-piece" -> DOMPseudoTypeScrollbarTrackPiece
                "scrollbar-corner" -> DOMPseudoTypeScrollbarCorner
                "resizer" -> DOMPseudoTypeResizer
                "input-list-button" -> DOMPseudoTypeInputListButton
                "page-transition" -> DOMPseudoTypePageTransition
                "page-transition-container" -> DOMPseudoTypePageTransitionContainer
                "page-transition-image-wrapper" -> DOMPseudoTypePageTransitionImageWrapper
                "page-transition-outgoing-image" -> DOMPseudoTypePageTransitionOutgoingImage
                "page-transition-incoming-image" -> DOMPseudoTypePageTransitionIncomingImage

instance ToJSON DOMPseudoType where
    toJSON v = A.String $
        case v of
                DOMPseudoTypeFirstLine -> "first-line"
                DOMPseudoTypeFirstLetter -> "first-letter"
                DOMPseudoTypeBefore -> "before"
                DOMPseudoTypeAfter -> "after"
                DOMPseudoTypeMarker -> "marker"
                DOMPseudoTypeBackdrop -> "backdrop"
                DOMPseudoTypeSelection -> "selection"
                DOMPseudoTypeTargetText -> "target-text"
                DOMPseudoTypeSpellingError -> "spelling-error"
                DOMPseudoTypeGrammarError -> "grammar-error"
                DOMPseudoTypeHighlight -> "highlight"
                DOMPseudoTypeFirstLineInherited -> "first-line-inherited"
                DOMPseudoTypeScrollbar -> "scrollbar"
                DOMPseudoTypeScrollbarThumb -> "scrollbar-thumb"
                DOMPseudoTypeScrollbarButton -> "scrollbar-button"
                DOMPseudoTypeScrollbarTrack -> "scrollbar-track"
                DOMPseudoTypeScrollbarTrackPiece -> "scrollbar-track-piece"
                DOMPseudoTypeScrollbarCorner -> "scrollbar-corner"
                DOMPseudoTypeResizer -> "resizer"
                DOMPseudoTypeInputListButton -> "input-list-button"
                DOMPseudoTypePageTransition -> "page-transition"
                DOMPseudoTypePageTransitionContainer -> "page-transition-container"
                DOMPseudoTypePageTransitionImageWrapper -> "page-transition-image-wrapper"
                DOMPseudoTypePageTransitionOutgoingImage -> "page-transition-outgoing-image"
                DOMPseudoTypePageTransitionIncomingImage -> "page-transition-incoming-image"



data DOMShadowRootType = DOMShadowRootTypeUserAgent | DOMShadowRootTypeOpen | DOMShadowRootTypeClosed
    deriving Show
instance FromJSON DOMShadowRootType where
    parseJSON = A.withText  "DOMShadowRootType"  $ \v -> 
        pure $ case v of
                "user-agent" -> DOMShadowRootTypeUserAgent
                "open" -> DOMShadowRootTypeOpen
                "closed" -> DOMShadowRootTypeClosed

instance ToJSON DOMShadowRootType where
    toJSON v = A.String $
        case v of
                DOMShadowRootTypeUserAgent -> "user-agent"
                DOMShadowRootTypeOpen -> "open"
                DOMShadowRootTypeClosed -> "closed"



data DOMCompatibilityMode = DOMCompatibilityModeQuirksMode | DOMCompatibilityModeLimitedQuirksMode | DOMCompatibilityModeNoQuirksMode
    deriving Show
instance FromJSON DOMCompatibilityMode where
    parseJSON = A.withText  "DOMCompatibilityMode"  $ \v -> 
        pure $ case v of
                "QuirksMode" -> DOMCompatibilityModeQuirksMode
                "LimitedQuirksMode" -> DOMCompatibilityModeLimitedQuirksMode
                "NoQuirksMode" -> DOMCompatibilityModeNoQuirksMode

instance ToJSON DOMCompatibilityMode where
    toJSON v = A.String $
        case v of
                DOMCompatibilityModeQuirksMode -> "QuirksMode"
                DOMCompatibilityModeLimitedQuirksMode -> "LimitedQuirksMode"
                DOMCompatibilityModeNoQuirksMode -> "NoQuirksMode"



data DOMNode = DOMNode {
    domNodeNodeId :: DOMNodeId,
    domNodeBackendNodeId :: DOMBackendNodeId,
    domNodeNodeType :: Int,
    domNodeNodeName :: String,
    domNodeLocalName :: String,
    domNodeNodeValue :: String,
    domNodeParentId :: Maybe DOMNodeId,
    domNodeChildNodeCount :: Maybe Int,
    domNodeChildren :: Maybe [DOMNode],
    domNodeAttributes :: Maybe [String],
    domNodeDocumentUrl :: Maybe String,
    domNodeBaseUrl :: Maybe String,
    domNodePublicId :: Maybe String,
    domNodeSystemId :: Maybe String,
    domNodeInternalSubset :: Maybe String,
    domNodeXmlVersion :: Maybe String,
    domNodeName :: Maybe String,
    domNodeValue :: Maybe String,
    domNodePseudoType :: Maybe DOMPseudoType,
    domNodeShadowRootType :: Maybe DOMShadowRootType,
    domNodeFrameId :: Maybe PageFrameId,
    domNodeContentDocument :: Maybe DOMNode,
    domNodeShadowRoots :: Maybe [DOMNode],
    domNodeTemplateContent :: Maybe DOMNode,
    domNodePseudoElements :: Maybe [DOMNode],
    domNodeDistributedNodes :: Maybe [DOMBackendNode],
    domNodeIsSvg :: Maybe Bool,
    domNodeCompatibilityMode :: Maybe DOMCompatibilityMode,
    domNodeAssignedSlot :: Maybe DOMBackendNode
} deriving Show
instance FromJSON  DOMNode where
    parseJSON = A.withObject "DOMNode" $ \v ->
         DOMNode <$> v .:  "nodeId"
            <*> v  .:  "backendNodeId"
            <*> v  .:  "nodeType"
            <*> v  .:  "nodeName"
            <*> v  .:  "localName"
            <*> v  .:  "nodeValue"
            <*> v  .:?  "parentId"
            <*> v  .:?  "childNodeCount"
            <*> v  .:?  "children"
            <*> v  .:?  "attributes"
            <*> v  .:?  "documentURL"
            <*> v  .:?  "baseURL"
            <*> v  .:?  "publicId"
            <*> v  .:?  "systemId"
            <*> v  .:?  "internalSubset"
            <*> v  .:?  "xmlVersion"
            <*> v  .:?  "name"
            <*> v  .:?  "value"
            <*> v  .:?  "pseudoType"
            <*> v  .:?  "shadowRootType"
            <*> v  .:?  "frameId"
            <*> v  .:?  "contentDocument"
            <*> v  .:?  "shadowRoots"
            <*> v  .:?  "templateContent"
            <*> v  .:?  "pseudoElements"
            <*> v  .:?  "distributedNodes"
            <*> v  .:?  "isSVG"
            <*> v  .:?  "compatibilityMode"
            <*> v  .:?  "assignedSlot"


instance ToJSON DOMNode  where
    toJSON v = A.object
        [ "nodeId" .= domNodeNodeId v
        , "backendNodeId" .= domNodeBackendNodeId v
        , "nodeType" .= domNodeNodeType v
        , "nodeName" .= domNodeNodeName v
        , "localName" .= domNodeLocalName v
        , "nodeValue" .= domNodeNodeValue v
        , "parentId" .= domNodeParentId v
        , "childNodeCount" .= domNodeChildNodeCount v
        , "children" .= domNodeChildren v
        , "attributes" .= domNodeAttributes v
        , "documentURL" .= domNodeDocumentUrl v
        , "baseURL" .= domNodeBaseUrl v
        , "publicId" .= domNodePublicId v
        , "systemId" .= domNodeSystemId v
        , "internalSubset" .= domNodeInternalSubset v
        , "xmlVersion" .= domNodeXmlVersion v
        , "name" .= domNodeName v
        , "value" .= domNodeValue v
        , "pseudoType" .= domNodePseudoType v
        , "shadowRootType" .= domNodeShadowRootType v
        , "frameId" .= domNodeFrameId v
        , "contentDocument" .= domNodeContentDocument v
        , "shadowRoots" .= domNodeShadowRoots v
        , "templateContent" .= domNodeTemplateContent v
        , "pseudoElements" .= domNodePseudoElements v
        , "distributedNodes" .= domNodeDistributedNodes v
        , "isSVG" .= domNodeIsSvg v
        , "compatibilityMode" .= domNodeCompatibilityMode v
        , "assignedSlot" .= domNodeAssignedSlot v
        ]



data DOMRGBA = DOMRGBA {
    domrgbaR :: Int,
    domrgbaG :: Int,
    domrgbaB :: Int,
    domrgbaA :: Maybe Int
} deriving Show
instance FromJSON  DOMRGBA where
    parseJSON = A.withObject "DOMRGBA" $ \v ->
         DOMRGBA <$> v .:  "r"
            <*> v  .:  "g"
            <*> v  .:  "b"
            <*> v  .:?  "a"


instance ToJSON DOMRGBA  where
    toJSON v = A.object
        [ "r" .= domrgbaR v
        , "g" .= domrgbaG v
        , "b" .= domrgbaB v
        , "a" .= domrgbaA v
        ]



type DOMQuad = [Int]

data DOMBoxModel = DOMBoxModel {
    domBoxModelContent :: DOMQuad,
    domBoxModelPadding :: DOMQuad,
    domBoxModelBorder :: DOMQuad,
    domBoxModelMargin :: DOMQuad,
    domBoxModelWidth :: Int,
    domBoxModelHeight :: Int,
    domBoxModelShapeOutside :: Maybe DOMShapeOutsideInfo
} deriving Show
instance FromJSON  DOMBoxModel where
    parseJSON = A.withObject "DOMBoxModel" $ \v ->
         DOMBoxModel <$> v .:  "content"
            <*> v  .:  "padding"
            <*> v  .:  "border"
            <*> v  .:  "margin"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:?  "shapeOutside"


instance ToJSON DOMBoxModel  where
    toJSON v = A.object
        [ "content" .= domBoxModelContent v
        , "padding" .= domBoxModelPadding v
        , "border" .= domBoxModelBorder v
        , "margin" .= domBoxModelMargin v
        , "width" .= domBoxModelWidth v
        , "height" .= domBoxModelHeight v
        , "shapeOutside" .= domBoxModelShapeOutside v
        ]



data DOMShapeOutsideInfo = DOMShapeOutsideInfo {
    domShapeOutsideInfoBounds :: DOMQuad,
    domShapeOutsideInfoShape :: [Int],
    domShapeOutsideInfoMarginShape :: [Int]
} deriving Show
instance FromJSON  DOMShapeOutsideInfo where
    parseJSON = A.withObject "DOMShapeOutsideInfo" $ \v ->
         DOMShapeOutsideInfo <$> v .:  "bounds"
            <*> v  .:  "shape"
            <*> v  .:  "marginShape"


instance ToJSON DOMShapeOutsideInfo  where
    toJSON v = A.object
        [ "bounds" .= domShapeOutsideInfoBounds v
        , "shape" .= domShapeOutsideInfoShape v
        , "marginShape" .= domShapeOutsideInfoMarginShape v
        ]



data DOMRect = DOMRect {
    domRectX :: Int,
    domRectY :: Int,
    domRectWidth :: Int,
    domRectHeight :: Int
} deriving Show
instance FromJSON  DOMRect where
    parseJSON = A.withObject "DOMRect" $ \v ->
         DOMRect <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"


instance ToJSON DOMRect  where
    toJSON v = A.object
        [ "x" .= domRectX v
        , "y" .= domRectY v
        , "width" .= domRectWidth v
        , "height" .= domRectHeight v
        ]



data DOMCSSComputedStyleProperty = DOMCSSComputedStyleProperty {
    domcssComputedStylePropertyName :: String,
    domcssComputedStylePropertyValue :: String
} deriving Show
instance FromJSON  DOMCSSComputedStyleProperty where
    parseJSON = A.withObject "DOMCSSComputedStyleProperty" $ \v ->
         DOMCSSComputedStyleProperty <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON DOMCSSComputedStyleProperty  where
    toJSON v = A.object
        [ "name" .= domcssComputedStylePropertyName v
        , "value" .= domcssComputedStylePropertyValue v
        ]


data DomDescribeNode = DomDescribeNode {
    domDescribeNodeNode :: DOMNode
} deriving Show
instance FromJSON  DomDescribeNode where
    parseJSON = A.withObject "DomDescribeNode" $ \v ->
         DomDescribeNode <$> v .:  "node"



domDescribeNode :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error DomDescribeNode)
domDescribeNode session domDescribeNodeNodeId domDescribeNodeBackendNodeId domDescribeNodeObjectId domDescribeNodeDepth domDescribeNodePierce = sendReceiveCommandResult (conn session) ("DOM","describeNode") ([] ++ (catMaybes [fmap (("domDescribeNodeNodeId",) . ToJSONEx) domDescribeNodeNodeId, fmap (("domDescribeNodeBackendNodeId",) . ToJSONEx) domDescribeNodeBackendNodeId, fmap (("domDescribeNodeObjectId",) . ToJSONEx) domDescribeNodeObjectId, fmap (("domDescribeNodeDepth",) . ToJSONEx) domDescribeNodeDepth, fmap (("domDescribeNodePierce",) . ToJSONEx) domDescribeNodePierce]))


domDisable :: Session -> IO (Maybe Error)
domDisable session  = sendReceiveCommand (conn session) ("DOM","disable") ([] ++ (catMaybes []))


domEnable :: Session -> IO (Maybe Error)
domEnable session  = sendReceiveCommand (conn session) ("DOM","enable") ([] ++ (catMaybes []))


domFocus :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Maybe Error)
domFocus session domFocusNodeId domFocusBackendNodeId domFocusObjectId = sendReceiveCommand (conn session) ("DOM","focus") ([] ++ (catMaybes [fmap (("domFocusNodeId",) . ToJSONEx) domFocusNodeId, fmap (("domFocusBackendNodeId",) . ToJSONEx) domFocusBackendNodeId, fmap (("domFocusObjectId",) . ToJSONEx) domFocusObjectId]))

data DomGetAttributes = DomGetAttributes {
    domGetAttributesAttributes :: [String]
} deriving Show
instance FromJSON  DomGetAttributes where
    parseJSON = A.withObject "DomGetAttributes" $ \v ->
         DomGetAttributes <$> v .:  "attributes"



domGetAttributes :: Session -> DOMNodeId -> IO (Either Error DomGetAttributes)
domGetAttributes session domGetAttributesNodeId = sendReceiveCommandResult (conn session) ("DOM","getAttributes") ([("domGetAttributesNodeId", ToJSONEx domGetAttributesNodeId)] ++ (catMaybes []))

data DomGetBoxModel = DomGetBoxModel {
    domGetBoxModelModel :: DOMBoxModel
} deriving Show
instance FromJSON  DomGetBoxModel where
    parseJSON = A.withObject "DomGetBoxModel" $ \v ->
         DomGetBoxModel <$> v .:  "model"



domGetBoxModel :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Either Error DomGetBoxModel)
domGetBoxModel session domGetBoxModelNodeId domGetBoxModelBackendNodeId domGetBoxModelObjectId = sendReceiveCommandResult (conn session) ("DOM","getBoxModel") ([] ++ (catMaybes [fmap (("domGetBoxModelNodeId",) . ToJSONEx) domGetBoxModelNodeId, fmap (("domGetBoxModelBackendNodeId",) . ToJSONEx) domGetBoxModelBackendNodeId, fmap (("domGetBoxModelObjectId",) . ToJSONEx) domGetBoxModelObjectId]))

data DomGetDocument = DomGetDocument {
    domGetDocumentRoot :: DOMNode
} deriving Show
instance FromJSON  DomGetDocument where
    parseJSON = A.withObject "DomGetDocument" $ \v ->
         DomGetDocument <$> v .:  "root"



domGetDocument :: Session -> Maybe Int -> Maybe Bool -> IO (Either Error DomGetDocument)
domGetDocument session domGetDocumentDepth domGetDocumentPierce = sendReceiveCommandResult (conn session) ("DOM","getDocument") ([] ++ (catMaybes [fmap (("domGetDocumentDepth",) . ToJSONEx) domGetDocumentDepth, fmap (("domGetDocumentPierce",) . ToJSONEx) domGetDocumentPierce]))

data DomGetNodeForLocation = DomGetNodeForLocation {
    domGetNodeForLocationBackendNodeId :: DOMBackendNodeId,
    domGetNodeForLocationFrameId :: PageFrameId,
    domGetNodeForLocationNodeId :: Maybe DOMNodeId
} deriving Show
instance FromJSON  DomGetNodeForLocation where
    parseJSON = A.withObject "DomGetNodeForLocation" $ \v ->
         DomGetNodeForLocation <$> v .:  "backendNodeId"
            <*> v  .:  "frameId"
            <*> v  .:?  "nodeId"



domGetNodeForLocation :: Session -> Int -> Int -> Maybe Bool -> Maybe Bool -> IO (Either Error DomGetNodeForLocation)
domGetNodeForLocation session domGetNodeForLocationX domGetNodeForLocationY domGetNodeForLocationIncludeUserAgentShadowDom domGetNodeForLocationIgnorePointerEventsNone = sendReceiveCommandResult (conn session) ("DOM","getNodeForLocation") ([("domGetNodeForLocationX", ToJSONEx domGetNodeForLocationX), ("domGetNodeForLocationY", ToJSONEx domGetNodeForLocationY)] ++ (catMaybes [fmap (("domGetNodeForLocationIncludeUserAgentShadowDom",) . ToJSONEx) domGetNodeForLocationIncludeUserAgentShadowDom, fmap (("domGetNodeForLocationIgnorePointerEventsNone",) . ToJSONEx) domGetNodeForLocationIgnorePointerEventsNone]))

data DomGetOuterHTML = DomGetOuterHTML {
    domGetOuterHTMLOuterHtml :: String
} deriving Show
instance FromJSON  DomGetOuterHTML where
    parseJSON = A.withObject "DomGetOuterHTML" $ \v ->
         DomGetOuterHTML <$> v .:  "outerHTML"



domGetOuterHTML :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Either Error DomGetOuterHTML)
domGetOuterHTML session domGetOuterHTMLNodeId domGetOuterHTMLBackendNodeId domGetOuterHTMLObjectId = sendReceiveCommandResult (conn session) ("DOM","getOuterHTML") ([] ++ (catMaybes [fmap (("domGetOuterHTMLNodeId",) . ToJSONEx) domGetOuterHTMLNodeId, fmap (("domGetOuterHTMLBackendNodeId",) . ToJSONEx) domGetOuterHTMLBackendNodeId, fmap (("domGetOuterHTMLObjectId",) . ToJSONEx) domGetOuterHTMLObjectId]))


domHideHighlight :: Session -> IO (Maybe Error)
domHideHighlight session  = sendReceiveCommand (conn session) ("DOM","hideHighlight") ([] ++ (catMaybes []))


domHighlightNode :: Session -> IO (Maybe Error)
domHighlightNode session  = sendReceiveCommand (conn session) ("DOM","highlightNode") ([] ++ (catMaybes []))


domHighlightRect :: Session -> IO (Maybe Error)
domHighlightRect session  = sendReceiveCommand (conn session) ("DOM","highlightRect") ([] ++ (catMaybes []))

data DomMoveTo = DomMoveTo {
    domMoveToNodeId :: DOMNodeId
} deriving Show
instance FromJSON  DomMoveTo where
    parseJSON = A.withObject "DomMoveTo" $ \v ->
         DomMoveTo <$> v .:  "nodeId"



domMoveTo :: Session -> DOMNodeId -> DOMNodeId -> Maybe DOMNodeId -> IO (Either Error DomMoveTo)
domMoveTo session domMoveToNodeId domMoveToTargetNodeId domMoveToInsertBeforeNodeId = sendReceiveCommandResult (conn session) ("DOM","moveTo") ([("domMoveToNodeId", ToJSONEx domMoveToNodeId), ("domMoveToTargetNodeId", ToJSONEx domMoveToTargetNodeId)] ++ (catMaybes [fmap (("domMoveToInsertBeforeNodeId",) . ToJSONEx) domMoveToInsertBeforeNodeId]))

data DomQuerySelector = DomQuerySelector {
    domQuerySelectorNodeId :: DOMNodeId
} deriving Show
instance FromJSON  DomQuerySelector where
    parseJSON = A.withObject "DomQuerySelector" $ \v ->
         DomQuerySelector <$> v .:  "nodeId"



domQuerySelector :: Session -> DOMNodeId -> String -> IO (Either Error DomQuerySelector)
domQuerySelector session domQuerySelectorNodeId domQuerySelectorSelector = sendReceiveCommandResult (conn session) ("DOM","querySelector") ([("domQuerySelectorNodeId", ToJSONEx domQuerySelectorNodeId), ("domQuerySelectorSelector", ToJSONEx domQuerySelectorSelector)] ++ (catMaybes []))

data DomQuerySelectorAll = DomQuerySelectorAll {
    domQuerySelectorAllNodeIds :: [DOMNodeId]
} deriving Show
instance FromJSON  DomQuerySelectorAll where
    parseJSON = A.withObject "DomQuerySelectorAll" $ \v ->
         DomQuerySelectorAll <$> v .:  "nodeIds"



domQuerySelectorAll :: Session -> DOMNodeId -> String -> IO (Either Error DomQuerySelectorAll)
domQuerySelectorAll session domQuerySelectorAllNodeId domQuerySelectorAllSelector = sendReceiveCommandResult (conn session) ("DOM","querySelectorAll") ([("domQuerySelectorAllNodeId", ToJSONEx domQuerySelectorAllNodeId), ("domQuerySelectorAllSelector", ToJSONEx domQuerySelectorAllSelector)] ++ (catMaybes []))


domRemoveAttribute :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domRemoveAttribute session domRemoveAttributeNodeId domRemoveAttributeName = sendReceiveCommand (conn session) ("DOM","removeAttribute") ([("domRemoveAttributeNodeId", ToJSONEx domRemoveAttributeNodeId), ("domRemoveAttributeName", ToJSONEx domRemoveAttributeName)] ++ (catMaybes []))


domRemoveNode :: Session -> DOMNodeId -> IO (Maybe Error)
domRemoveNode session domRemoveNodeNodeId = sendReceiveCommand (conn session) ("DOM","removeNode") ([("domRemoveNodeNodeId", ToJSONEx domRemoveNodeNodeId)] ++ (catMaybes []))


domRequestChildNodes :: Session -> DOMNodeId -> Maybe Int -> Maybe Bool -> IO (Maybe Error)
domRequestChildNodes session domRequestChildNodesNodeId domRequestChildNodesDepth domRequestChildNodesPierce = sendReceiveCommand (conn session) ("DOM","requestChildNodes") ([("domRequestChildNodesNodeId", ToJSONEx domRequestChildNodesNodeId)] ++ (catMaybes [fmap (("domRequestChildNodesDepth",) . ToJSONEx) domRequestChildNodesDepth, fmap (("domRequestChildNodesPierce",) . ToJSONEx) domRequestChildNodesPierce]))

data DomRequestNode = DomRequestNode {
    domRequestNodeNodeId :: DOMNodeId
} deriving Show
instance FromJSON  DomRequestNode where
    parseJSON = A.withObject "DomRequestNode" $ \v ->
         DomRequestNode <$> v .:  "nodeId"



domRequestNode :: Session -> RuntimeRemoteObjectId -> IO (Either Error DomRequestNode)
domRequestNode session domRequestNodeObjectId = sendReceiveCommandResult (conn session) ("DOM","requestNode") ([("domRequestNodeObjectId", ToJSONEx domRequestNodeObjectId)] ++ (catMaybes []))

data DomResolveNode = DomResolveNode {
    domResolveNodeObject :: RuntimeRemoteObject
} deriving Show
instance FromJSON  DomResolveNode where
    parseJSON = A.withObject "DomResolveNode" $ \v ->
         DomResolveNode <$> v .:  "object"



domResolveNode :: Session -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe String -> Maybe RuntimeExecutionContextId -> IO (Either Error DomResolveNode)
domResolveNode session domResolveNodeNodeId domResolveNodeBackendNodeId domResolveNodeObjectGroup domResolveNodeExecutionContextId = sendReceiveCommandResult (conn session) ("DOM","resolveNode") ([] ++ (catMaybes [fmap (("domResolveNodeNodeId",) . ToJSONEx) domResolveNodeNodeId, fmap (("domResolveNodeBackendNodeId",) . ToJSONEx) domResolveNodeBackendNodeId, fmap (("domResolveNodeObjectGroup",) . ToJSONEx) domResolveNodeObjectGroup, fmap (("domResolveNodeExecutionContextId",) . ToJSONEx) domResolveNodeExecutionContextId]))


domSetAttributeValue :: Session -> DOMNodeId -> String -> String -> IO (Maybe Error)
domSetAttributeValue session domSetAttributeValueNodeId domSetAttributeValueName domSetAttributeValueValue = sendReceiveCommand (conn session) ("DOM","setAttributeValue") ([("domSetAttributeValueNodeId", ToJSONEx domSetAttributeValueNodeId), ("domSetAttributeValueName", ToJSONEx domSetAttributeValueName), ("domSetAttributeValueValue", ToJSONEx domSetAttributeValueValue)] ++ (catMaybes []))


domSetAttributesAsText :: Session -> DOMNodeId -> String -> Maybe String -> IO (Maybe Error)
domSetAttributesAsText session domSetAttributesAsTextNodeId domSetAttributesAsTextText domSetAttributesAsTextName = sendReceiveCommand (conn session) ("DOM","setAttributesAsText") ([("domSetAttributesAsTextNodeId", ToJSONEx domSetAttributesAsTextNodeId), ("domSetAttributesAsTextText", ToJSONEx domSetAttributesAsTextText)] ++ (catMaybes [fmap (("domSetAttributesAsTextName",) . ToJSONEx) domSetAttributesAsTextName]))


domSetFileInputFiles :: Session -> [String] -> Maybe DOMNodeId -> Maybe DOMBackendNodeId -> Maybe RuntimeRemoteObjectId -> IO (Maybe Error)
domSetFileInputFiles session domSetFileInputFilesFiles domSetFileInputFilesNodeId domSetFileInputFilesBackendNodeId domSetFileInputFilesObjectId = sendReceiveCommand (conn session) ("DOM","setFileInputFiles") ([("domSetFileInputFilesFiles", ToJSONEx domSetFileInputFilesFiles)] ++ (catMaybes [fmap (("domSetFileInputFilesNodeId",) . ToJSONEx) domSetFileInputFilesNodeId, fmap (("domSetFileInputFilesBackendNodeId",) . ToJSONEx) domSetFileInputFilesBackendNodeId, fmap (("domSetFileInputFilesObjectId",) . ToJSONEx) domSetFileInputFilesObjectId]))

data DomSetNodeName = DomSetNodeName {
    domSetNodeNameNodeId :: DOMNodeId
} deriving Show
instance FromJSON  DomSetNodeName where
    parseJSON = A.withObject "DomSetNodeName" $ \v ->
         DomSetNodeName <$> v .:  "nodeId"



domSetNodeName :: Session -> DOMNodeId -> String -> IO (Either Error DomSetNodeName)
domSetNodeName session domSetNodeNameNodeId domSetNodeNameName = sendReceiveCommandResult (conn session) ("DOM","setNodeName") ([("domSetNodeNameNodeId", ToJSONEx domSetNodeNameNodeId), ("domSetNodeNameName", ToJSONEx domSetNodeNameName)] ++ (catMaybes []))


domSetNodeValue :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domSetNodeValue session domSetNodeValueNodeId domSetNodeValueValue = sendReceiveCommand (conn session) ("DOM","setNodeValue") ([("domSetNodeValueNodeId", ToJSONEx domSetNodeValueNodeId), ("domSetNodeValueValue", ToJSONEx domSetNodeValueValue)] ++ (catMaybes []))


domSetOuterHTML :: Session -> DOMNodeId -> String -> IO (Maybe Error)
domSetOuterHTML session domSetOuterHTMLNodeId domSetOuterHTMLOuterHtml = sendReceiveCommand (conn session) ("DOM","setOuterHTML") ([("domSetOuterHTMLNodeId", ToJSONEx domSetOuterHTMLNodeId), ("domSetOuterHTMLOuterHtml", ToJSONEx domSetOuterHTMLOuterHtml)] ++ (catMaybes []))



data DOMDebuggerDOMBreakpointType = DOMDebuggerDOMBreakpointTypeSubtreeModified | DOMDebuggerDOMBreakpointTypeAttributeModified | DOMDebuggerDOMBreakpointTypeNodeRemoved
    deriving Show
instance FromJSON DOMDebuggerDOMBreakpointType where
    parseJSON = A.withText  "DOMDebuggerDOMBreakpointType"  $ \v -> 
        pure $ case v of
                "subtree-modified" -> DOMDebuggerDOMBreakpointTypeSubtreeModified
                "attribute-modified" -> DOMDebuggerDOMBreakpointTypeAttributeModified
                "node-removed" -> DOMDebuggerDOMBreakpointTypeNodeRemoved

instance ToJSON DOMDebuggerDOMBreakpointType where
    toJSON v = A.String $
        case v of
                DOMDebuggerDOMBreakpointTypeSubtreeModified -> "subtree-modified"
                DOMDebuggerDOMBreakpointTypeAttributeModified -> "attribute-modified"
                DOMDebuggerDOMBreakpointTypeNodeRemoved -> "node-removed"



data DOMDebuggerEventListener = DOMDebuggerEventListener {
    domDebuggerEventListenerType :: String,
    domDebuggerEventListenerUseCapture :: Bool,
    domDebuggerEventListenerPassive :: Bool,
    domDebuggerEventListenerOnce :: Bool,
    domDebuggerEventListenerScriptId :: RuntimeScriptId,
    domDebuggerEventListenerLineNumber :: Int,
    domDebuggerEventListenerColumnNumber :: Int,
    domDebuggerEventListenerHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerOriginalHandler :: Maybe RuntimeRemoteObject,
    domDebuggerEventListenerBackendNodeId :: Maybe DOMBackendNodeId
} deriving Show
instance FromJSON  DOMDebuggerEventListener where
    parseJSON = A.withObject "DOMDebuggerEventListener" $ \v ->
         DOMDebuggerEventListener <$> v .:  "type"
            <*> v  .:  "useCapture"
            <*> v  .:  "passive"
            <*> v  .:  "once"
            <*> v  .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "handler"
            <*> v  .:?  "originalHandler"
            <*> v  .:?  "backendNodeId"


instance ToJSON DOMDebuggerEventListener  where
    toJSON v = A.object
        [ "type" .= domDebuggerEventListenerType v
        , "useCapture" .= domDebuggerEventListenerUseCapture v
        , "passive" .= domDebuggerEventListenerPassive v
        , "once" .= domDebuggerEventListenerOnce v
        , "scriptId" .= domDebuggerEventListenerScriptId v
        , "lineNumber" .= domDebuggerEventListenerLineNumber v
        , "columnNumber" .= domDebuggerEventListenerColumnNumber v
        , "handler" .= domDebuggerEventListenerHandler v
        , "originalHandler" .= domDebuggerEventListenerOriginalHandler v
        , "backendNodeId" .= domDebuggerEventListenerBackendNodeId v
        ]


data DomDebuggerGetEventListeners = DomDebuggerGetEventListeners {
    domDebuggerGetEventListenersListeners :: [DOMDebuggerEventListener]
} deriving Show
instance FromJSON  DomDebuggerGetEventListeners where
    parseJSON = A.withObject "DomDebuggerGetEventListeners" $ \v ->
         DomDebuggerGetEventListeners <$> v .:  "listeners"



domDebuggerGetEventListeners :: Session -> RuntimeRemoteObjectId -> Maybe Int -> Maybe Bool -> IO (Either Error DomDebuggerGetEventListeners)
domDebuggerGetEventListeners session domDebuggerGetEventListenersObjectId domDebuggerGetEventListenersDepth domDebuggerGetEventListenersPierce = sendReceiveCommandResult (conn session) ("DOMDebugger","getEventListeners") ([("domDebuggerGetEventListenersObjectId", ToJSONEx domDebuggerGetEventListenersObjectId)] ++ (catMaybes [fmap (("domDebuggerGetEventListenersDepth",) . ToJSONEx) domDebuggerGetEventListenersDepth, fmap (("domDebuggerGetEventListenersPierce",) . ToJSONEx) domDebuggerGetEventListenersPierce]))


domDebuggerRemoveDOMBreakpoint :: Session -> DOMNodeId -> DOMDebuggerDOMBreakpointType -> IO (Maybe Error)
domDebuggerRemoveDOMBreakpoint session domDebuggerRemoveDOMBreakpointNodeId domDebuggerRemoveDOMBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","removeDOMBreakpoint") ([("domDebuggerRemoveDOMBreakpointNodeId", ToJSONEx domDebuggerRemoveDOMBreakpointNodeId), ("domDebuggerRemoveDOMBreakpointType", ToJSONEx domDebuggerRemoveDOMBreakpointType)] ++ (catMaybes []))


domDebuggerRemoveEventListenerBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerRemoveEventListenerBreakpoint session domDebuggerRemoveEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","removeEventListenerBreakpoint") ([("domDebuggerRemoveEventListenerBreakpointEventName", ToJSONEx domDebuggerRemoveEventListenerBreakpointEventName)] ++ (catMaybes []))


domDebuggerRemoveXHRBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerRemoveXHRBreakpoint session domDebuggerRemoveXHRBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","removeXHRBreakpoint") ([("domDebuggerRemoveXHRBreakpointUrl", ToJSONEx domDebuggerRemoveXHRBreakpointUrl)] ++ (catMaybes []))


domDebuggerSetDOMBreakpoint :: Session -> DOMNodeId -> DOMDebuggerDOMBreakpointType -> IO (Maybe Error)
domDebuggerSetDOMBreakpoint session domDebuggerSetDOMBreakpointNodeId domDebuggerSetDOMBreakpointType = sendReceiveCommand (conn session) ("DOMDebugger","setDOMBreakpoint") ([("domDebuggerSetDOMBreakpointNodeId", ToJSONEx domDebuggerSetDOMBreakpointNodeId), ("domDebuggerSetDOMBreakpointType", ToJSONEx domDebuggerSetDOMBreakpointType)] ++ (catMaybes []))


domDebuggerSetEventListenerBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerSetEventListenerBreakpoint session domDebuggerSetEventListenerBreakpointEventName = sendReceiveCommand (conn session) ("DOMDebugger","setEventListenerBreakpoint") ([("domDebuggerSetEventListenerBreakpointEventName", ToJSONEx domDebuggerSetEventListenerBreakpointEventName)] ++ (catMaybes []))


domDebuggerSetXHRBreakpoint :: Session -> String -> IO (Maybe Error)
domDebuggerSetXHRBreakpoint session domDebuggerSetXHRBreakpointUrl = sendReceiveCommand (conn session) ("DOMDebugger","setXHRBreakpoint") ([("domDebuggerSetXHRBreakpointUrl", ToJSONEx domDebuggerSetXHRBreakpointUrl)] ++ (catMaybes []))



data EmulationScreenOrientation = EmulationScreenOrientation {
    emulationScreenOrientationType :: String,
    emulationScreenOrientationAngle :: Int
} deriving Show
instance FromJSON  EmulationScreenOrientation where
    parseJSON = A.withObject "EmulationScreenOrientation" $ \v ->
         EmulationScreenOrientation <$> v .:  "type"
            <*> v  .:  "angle"


instance ToJSON EmulationScreenOrientation  where
    toJSON v = A.object
        [ "type" .= emulationScreenOrientationType v
        , "angle" .= emulationScreenOrientationAngle v
        ]



data EmulationDisplayFeature = EmulationDisplayFeature {
    emulationDisplayFeatureOrientation :: String,
    emulationDisplayFeatureOffset :: Int,
    emulationDisplayFeatureMaskLength :: Int
} deriving Show
instance FromJSON  EmulationDisplayFeature where
    parseJSON = A.withObject "EmulationDisplayFeature" $ \v ->
         EmulationDisplayFeature <$> v .:  "orientation"
            <*> v  .:  "offset"
            <*> v  .:  "maskLength"


instance ToJSON EmulationDisplayFeature  where
    toJSON v = A.object
        [ "orientation" .= emulationDisplayFeatureOrientation v
        , "offset" .= emulationDisplayFeatureOffset v
        , "maskLength" .= emulationDisplayFeatureMaskLength v
        ]



data EmulationMediaFeature = EmulationMediaFeature {
    emulationMediaFeatureName :: String,
    emulationMediaFeatureValue :: String
} deriving Show
instance FromJSON  EmulationMediaFeature where
    parseJSON = A.withObject "EmulationMediaFeature" $ \v ->
         EmulationMediaFeature <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON EmulationMediaFeature  where
    toJSON v = A.object
        [ "name" .= emulationMediaFeatureName v
        , "value" .= emulationMediaFeatureValue v
        ]


data EmulationCanEmulate = EmulationCanEmulate {
    emulationCanEmulateResult :: Bool
} deriving Show
instance FromJSON  EmulationCanEmulate where
    parseJSON = A.withObject "EmulationCanEmulate" $ \v ->
         EmulationCanEmulate <$> v .:  "result"



emulationCanEmulate :: Session -> IO (Either Error EmulationCanEmulate)
emulationCanEmulate session  = sendReceiveCommandResult (conn session) ("Emulation","canEmulate") ([] ++ (catMaybes []))


emulationClearDeviceMetricsOverride :: Session -> IO (Maybe Error)
emulationClearDeviceMetricsOverride session  = sendReceiveCommand (conn session) ("Emulation","clearDeviceMetricsOverride") ([] ++ (catMaybes []))


emulationClearGeolocationOverride :: Session -> IO (Maybe Error)
emulationClearGeolocationOverride session  = sendReceiveCommand (conn session) ("Emulation","clearGeolocationOverride") ([] ++ (catMaybes []))


emulationSetDefaultBackgroundColorOverride :: Session -> Maybe DOMRGBA -> IO (Maybe Error)
emulationSetDefaultBackgroundColorOverride session emulationSetDefaultBackgroundColorOverrideColor = sendReceiveCommand (conn session) ("Emulation","setDefaultBackgroundColorOverride") ([] ++ (catMaybes [fmap (("emulationSetDefaultBackgroundColorOverrideColor",) . ToJSONEx) emulationSetDefaultBackgroundColorOverrideColor]))


emulationSetDeviceMetricsOverride :: Session -> Int -> Int -> Int -> Bool -> Maybe EmulationScreenOrientation -> IO (Maybe Error)
emulationSetDeviceMetricsOverride session emulationSetDeviceMetricsOverrideWidth emulationSetDeviceMetricsOverrideHeight emulationSetDeviceMetricsOverrideDeviceScaleFactor emulationSetDeviceMetricsOverrideMobile emulationSetDeviceMetricsOverrideScreenOrientation = sendReceiveCommand (conn session) ("Emulation","setDeviceMetricsOverride") ([("emulationSetDeviceMetricsOverrideWidth", ToJSONEx emulationSetDeviceMetricsOverrideWidth), ("emulationSetDeviceMetricsOverrideHeight", ToJSONEx emulationSetDeviceMetricsOverrideHeight), ("emulationSetDeviceMetricsOverrideDeviceScaleFactor", ToJSONEx emulationSetDeviceMetricsOverrideDeviceScaleFactor), ("emulationSetDeviceMetricsOverrideMobile", ToJSONEx emulationSetDeviceMetricsOverrideMobile)] ++ (catMaybes [fmap (("emulationSetDeviceMetricsOverrideScreenOrientation",) . ToJSONEx) emulationSetDeviceMetricsOverrideScreenOrientation]))


emulationSetEmulatedMedia :: Session -> Maybe String -> Maybe [EmulationMediaFeature] -> IO (Maybe Error)
emulationSetEmulatedMedia session emulationSetEmulatedMediaMedia emulationSetEmulatedMediaFeatures = sendReceiveCommand (conn session) ("Emulation","setEmulatedMedia") ([] ++ (catMaybes [fmap (("emulationSetEmulatedMediaMedia",) . ToJSONEx) emulationSetEmulatedMediaMedia, fmap (("emulationSetEmulatedMediaFeatures",) . ToJSONEx) emulationSetEmulatedMediaFeatures]))


emulationSetGeolocationOverride :: Session -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Maybe Error)
emulationSetGeolocationOverride session emulationSetGeolocationOverrideLatitude emulationSetGeolocationOverrideLongitude emulationSetGeolocationOverrideAccuracy = sendReceiveCommand (conn session) ("Emulation","setGeolocationOverride") ([] ++ (catMaybes [fmap (("emulationSetGeolocationOverrideLatitude",) . ToJSONEx) emulationSetGeolocationOverrideLatitude, fmap (("emulationSetGeolocationOverrideLongitude",) . ToJSONEx) emulationSetGeolocationOverrideLongitude, fmap (("emulationSetGeolocationOverrideAccuracy",) . ToJSONEx) emulationSetGeolocationOverrideAccuracy]))


emulationSetScriptExecutionDisabled :: Session -> Bool -> IO (Maybe Error)
emulationSetScriptExecutionDisabled session emulationSetScriptExecutionDisabledValue = sendReceiveCommand (conn session) ("Emulation","setScriptExecutionDisabled") ([("emulationSetScriptExecutionDisabledValue", ToJSONEx emulationSetScriptExecutionDisabledValue)] ++ (catMaybes []))


emulationSetTouchEmulationEnabled :: Session -> Bool -> Maybe Int -> IO (Maybe Error)
emulationSetTouchEmulationEnabled session emulationSetTouchEmulationEnabledEnabled emulationSetTouchEmulationEnabledMaxTouchPoints = sendReceiveCommand (conn session) ("Emulation","setTouchEmulationEnabled") ([("emulationSetTouchEmulationEnabledEnabled", ToJSONEx emulationSetTouchEmulationEnabledEnabled)] ++ (catMaybes [fmap (("emulationSetTouchEmulationEnabledMaxTouchPoints",) . ToJSONEx) emulationSetTouchEmulationEnabledMaxTouchPoints]))


emulationSetUserAgentOverride :: Session -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
emulationSetUserAgentOverride session emulationSetUserAgentOverrideUserAgent emulationSetUserAgentOverrideAcceptLanguage emulationSetUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Emulation","setUserAgentOverride") ([("emulationSetUserAgentOverrideUserAgent", ToJSONEx emulationSetUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("emulationSetUserAgentOverrideAcceptLanguage",) . ToJSONEx) emulationSetUserAgentOverrideAcceptLanguage, fmap (("emulationSetUserAgentOverridePlatform",) . ToJSONEx) emulationSetUserAgentOverridePlatform]))



type IOStreamHandle = String

ioClose :: Session -> IOStreamHandle -> IO (Maybe Error)
ioClose session ioCloseHandle = sendReceiveCommand (conn session) ("IO","close") ([("ioCloseHandle", ToJSONEx ioCloseHandle)] ++ (catMaybes []))

data IoRead = IoRead {
    ioReadData :: String,
    ioReadEof :: Bool,
    ioReadBase64Encoded :: Maybe Bool
} deriving Show
instance FromJSON  IoRead where
    parseJSON = A.withObject "IoRead" $ \v ->
         IoRead <$> v .:  "data"
            <*> v  .:  "eof"
            <*> v  .:?  "base64Encoded"



ioRead :: Session -> IOStreamHandle -> Maybe Int -> Maybe Int -> IO (Either Error IoRead)
ioRead session ioReadHandle ioReadOffset ioReadSize = sendReceiveCommandResult (conn session) ("IO","read") ([("ioReadHandle", ToJSONEx ioReadHandle)] ++ (catMaybes [fmap (("ioReadOffset",) . ToJSONEx) ioReadOffset, fmap (("ioReadSize",) . ToJSONEx) ioReadSize]))

data IoResolveBlob = IoResolveBlob {
    ioResolveBlobUuid :: String
} deriving Show
instance FromJSON  IoResolveBlob where
    parseJSON = A.withObject "IoResolveBlob" $ \v ->
         IoResolveBlob <$> v .:  "uuid"



ioResolveBlob :: Session -> RuntimeRemoteObjectId -> IO (Either Error IoResolveBlob)
ioResolveBlob session ioResolveBlobObjectId = sendReceiveCommandResult (conn session) ("IO","resolveBlob") ([("ioResolveBlobObjectId", ToJSONEx ioResolveBlobObjectId)] ++ (catMaybes []))



data InputTouchPoint = InputTouchPoint {
    inputTouchPointX :: Int,
    inputTouchPointY :: Int,
    inputTouchPointRadiusX :: Maybe Int,
    inputTouchPointRadiusY :: Maybe Int,
    inputTouchPointRotationAngle :: Maybe Int,
    inputTouchPointForce :: Maybe Int,
    inputTouchPointId :: Maybe Int
} deriving Show
instance FromJSON  InputTouchPoint where
    parseJSON = A.withObject "InputTouchPoint" $ \v ->
         InputTouchPoint <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:?  "radiusX"
            <*> v  .:?  "radiusY"
            <*> v  .:?  "rotationAngle"
            <*> v  .:?  "force"
            <*> v  .:?  "id"


instance ToJSON InputTouchPoint  where
    toJSON v = A.object
        [ "x" .= inputTouchPointX v
        , "y" .= inputTouchPointY v
        , "radiusX" .= inputTouchPointRadiusX v
        , "radiusY" .= inputTouchPointRadiusY v
        , "rotationAngle" .= inputTouchPointRotationAngle v
        , "force" .= inputTouchPointForce v
        , "id" .= inputTouchPointId v
        ]



data InputMouseButton = InputMouseButtonNone | InputMouseButtonLeft | InputMouseButtonMiddle | InputMouseButtonRight | InputMouseButtonBack | InputMouseButtonForward
    deriving Show
instance FromJSON InputMouseButton where
    parseJSON = A.withText  "InputMouseButton"  $ \v -> 
        pure $ case v of
                "none" -> InputMouseButtonNone
                "left" -> InputMouseButtonLeft
                "middle" -> InputMouseButtonMiddle
                "right" -> InputMouseButtonRight
                "back" -> InputMouseButtonBack
                "forward" -> InputMouseButtonForward

instance ToJSON InputMouseButton where
    toJSON v = A.String $
        case v of
                InputMouseButtonNone -> "none"
                InputMouseButtonLeft -> "left"
                InputMouseButtonMiddle -> "middle"
                InputMouseButtonRight -> "right"
                InputMouseButtonBack -> "back"
                InputMouseButtonForward -> "forward"



type InputTimeSinceEpoch = Int

inputDispatchKeyEvent :: Session -> String -> Maybe Int -> Maybe InputTimeSinceEpoch -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> IO (Maybe Error)
inputDispatchKeyEvent session inputDispatchKeyEventType inputDispatchKeyEventModifiers inputDispatchKeyEventTimestamp inputDispatchKeyEventText inputDispatchKeyEventUnmodifiedText inputDispatchKeyEventKeyIdentifier inputDispatchKeyEventCode inputDispatchKeyEventKey inputDispatchKeyEventWindowsVirtualKeyCode inputDispatchKeyEventNativeVirtualKeyCode inputDispatchKeyEventAutoRepeat inputDispatchKeyEventIsKeypad inputDispatchKeyEventIsSystemKey inputDispatchKeyEventLocation = sendReceiveCommand (conn session) ("Input","dispatchKeyEvent") ([("inputDispatchKeyEventType", ToJSONEx inputDispatchKeyEventType)] ++ (catMaybes [fmap (("inputDispatchKeyEventModifiers",) . ToJSONEx) inputDispatchKeyEventModifiers, fmap (("inputDispatchKeyEventTimestamp",) . ToJSONEx) inputDispatchKeyEventTimestamp, fmap (("inputDispatchKeyEventText",) . ToJSONEx) inputDispatchKeyEventText, fmap (("inputDispatchKeyEventUnmodifiedText",) . ToJSONEx) inputDispatchKeyEventUnmodifiedText, fmap (("inputDispatchKeyEventKeyIdentifier",) . ToJSONEx) inputDispatchKeyEventKeyIdentifier, fmap (("inputDispatchKeyEventCode",) . ToJSONEx) inputDispatchKeyEventCode, fmap (("inputDispatchKeyEventKey",) . ToJSONEx) inputDispatchKeyEventKey, fmap (("inputDispatchKeyEventWindowsVirtualKeyCode",) . ToJSONEx) inputDispatchKeyEventWindowsVirtualKeyCode, fmap (("inputDispatchKeyEventNativeVirtualKeyCode",) . ToJSONEx) inputDispatchKeyEventNativeVirtualKeyCode, fmap (("inputDispatchKeyEventAutoRepeat",) . ToJSONEx) inputDispatchKeyEventAutoRepeat, fmap (("inputDispatchKeyEventIsKeypad",) . ToJSONEx) inputDispatchKeyEventIsKeypad, fmap (("inputDispatchKeyEventIsSystemKey",) . ToJSONEx) inputDispatchKeyEventIsSystemKey, fmap (("inputDispatchKeyEventLocation",) . ToJSONEx) inputDispatchKeyEventLocation]))


inputDispatchMouseEvent :: Session -> String -> Int -> Int -> Maybe Int -> Maybe InputTimeSinceEpoch -> Maybe InputMouseButton -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> IO (Maybe Error)
inputDispatchMouseEvent session inputDispatchMouseEventType inputDispatchMouseEventX inputDispatchMouseEventY inputDispatchMouseEventModifiers inputDispatchMouseEventTimestamp inputDispatchMouseEventButton inputDispatchMouseEventButtons inputDispatchMouseEventClickCount inputDispatchMouseEventDeltaX inputDispatchMouseEventDeltaY inputDispatchMouseEventPointerType = sendReceiveCommand (conn session) ("Input","dispatchMouseEvent") ([("inputDispatchMouseEventType", ToJSONEx inputDispatchMouseEventType), ("inputDispatchMouseEventX", ToJSONEx inputDispatchMouseEventX), ("inputDispatchMouseEventY", ToJSONEx inputDispatchMouseEventY)] ++ (catMaybes [fmap (("inputDispatchMouseEventModifiers",) . ToJSONEx) inputDispatchMouseEventModifiers, fmap (("inputDispatchMouseEventTimestamp",) . ToJSONEx) inputDispatchMouseEventTimestamp, fmap (("inputDispatchMouseEventButton",) . ToJSONEx) inputDispatchMouseEventButton, fmap (("inputDispatchMouseEventButtons",) . ToJSONEx) inputDispatchMouseEventButtons, fmap (("inputDispatchMouseEventClickCount",) . ToJSONEx) inputDispatchMouseEventClickCount, fmap (("inputDispatchMouseEventDeltaX",) . ToJSONEx) inputDispatchMouseEventDeltaX, fmap (("inputDispatchMouseEventDeltaY",) . ToJSONEx) inputDispatchMouseEventDeltaY, fmap (("inputDispatchMouseEventPointerType",) . ToJSONEx) inputDispatchMouseEventPointerType]))


inputDispatchTouchEvent :: Session -> String -> [InputTouchPoint] -> Maybe Int -> Maybe InputTimeSinceEpoch -> IO (Maybe Error)
inputDispatchTouchEvent session inputDispatchTouchEventType inputDispatchTouchEventTouchPoints inputDispatchTouchEventModifiers inputDispatchTouchEventTimestamp = sendReceiveCommand (conn session) ("Input","dispatchTouchEvent") ([("inputDispatchTouchEventType", ToJSONEx inputDispatchTouchEventType), ("inputDispatchTouchEventTouchPoints", ToJSONEx inputDispatchTouchEventTouchPoints)] ++ (catMaybes [fmap (("inputDispatchTouchEventModifiers",) . ToJSONEx) inputDispatchTouchEventModifiers, fmap (("inputDispatchTouchEventTimestamp",) . ToJSONEx) inputDispatchTouchEventTimestamp]))


inputSetIgnoreInputEvents :: Session -> Bool -> IO (Maybe Error)
inputSetIgnoreInputEvents session inputSetIgnoreInputEventsIgnore = sendReceiveCommand (conn session) ("Input","setIgnoreInputEvents") ([("inputSetIgnoreInputEventsIgnore", ToJSONEx inputSetIgnoreInputEventsIgnore)] ++ (catMaybes []))



data LogLogEntry = LogLogEntry {
    logLogEntrySource :: String,
    logLogEntryLevel :: String,
    logLogEntryText :: String,
    logLogEntryTimestamp :: RuntimeTimestamp,
    logLogEntryCategory :: Maybe String,
    logLogEntryUrl :: Maybe String,
    logLogEntryLineNumber :: Maybe Int,
    logLogEntryStackTrace :: Maybe RuntimeStackTrace,
    logLogEntryNetworkRequestId :: Maybe NetworkRequestId,
    logLogEntryWorkerId :: Maybe String,
    logLogEntryArgs :: Maybe [RuntimeRemoteObject]
} deriving Show
instance FromJSON  LogLogEntry where
    parseJSON = A.withObject "LogLogEntry" $ \v ->
         LogLogEntry <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:  "timestamp"
            <*> v  .:?  "category"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "networkRequestId"
            <*> v  .:?  "workerId"
            <*> v  .:?  "args"


instance ToJSON LogLogEntry  where
    toJSON v = A.object
        [ "source" .= logLogEntrySource v
        , "level" .= logLogEntryLevel v
        , "text" .= logLogEntryText v
        , "timestamp" .= logLogEntryTimestamp v
        , "category" .= logLogEntryCategory v
        , "url" .= logLogEntryUrl v
        , "lineNumber" .= logLogEntryLineNumber v
        , "stackTrace" .= logLogEntryStackTrace v
        , "networkRequestId" .= logLogEntryNetworkRequestId v
        , "workerId" .= logLogEntryWorkerId v
        , "args" .= logLogEntryArgs v
        ]



data LogViolationSetting = LogViolationSetting {
    logViolationSettingName :: String,
    logViolationSettingThreshold :: Int
} deriving Show
instance FromJSON  LogViolationSetting where
    parseJSON = A.withObject "LogViolationSetting" $ \v ->
         LogViolationSetting <$> v .:  "name"
            <*> v  .:  "threshold"


instance ToJSON LogViolationSetting  where
    toJSON v = A.object
        [ "name" .= logViolationSettingName v
        , "threshold" .= logViolationSettingThreshold v
        ]



logClear :: Session -> IO (Maybe Error)
logClear session  = sendReceiveCommand (conn session) ("Log","clear") ([] ++ (catMaybes []))


logDisable :: Session -> IO (Maybe Error)
logDisable session  = sendReceiveCommand (conn session) ("Log","disable") ([] ++ (catMaybes []))


logEnable :: Session -> IO (Maybe Error)
logEnable session  = sendReceiveCommand (conn session) ("Log","enable") ([] ++ (catMaybes []))


logStartViolationsReport :: Session -> [LogViolationSetting] -> IO (Maybe Error)
logStartViolationsReport session logStartViolationsReportConfig = sendReceiveCommand (conn session) ("Log","startViolationsReport") ([("logStartViolationsReportConfig", ToJSONEx logStartViolationsReportConfig)] ++ (catMaybes []))


logStopViolationsReport :: Session -> IO (Maybe Error)
logStopViolationsReport session  = sendReceiveCommand (conn session) ("Log","stopViolationsReport") ([] ++ (catMaybes []))



data NetworkResourceType = NetworkResourceTypeDocument | NetworkResourceTypeStylesheet | NetworkResourceTypeImage | NetworkResourceTypeMedia | NetworkResourceTypeFont | NetworkResourceTypeScript | NetworkResourceTypeTextTrack | NetworkResourceTypeXhr | NetworkResourceTypeFetch | NetworkResourceTypeEventSource | NetworkResourceTypeWebSocket | NetworkResourceTypeManifest | NetworkResourceTypeSignedExchange | NetworkResourceTypePing | NetworkResourceTypeCspViolationReport | NetworkResourceTypePreflight | NetworkResourceTypeOther
    deriving Show
instance FromJSON NetworkResourceType where
    parseJSON = A.withText  "NetworkResourceType"  $ \v -> 
        pure $ case v of
                "Document" -> NetworkResourceTypeDocument
                "Stylesheet" -> NetworkResourceTypeStylesheet
                "Image" -> NetworkResourceTypeImage
                "Media" -> NetworkResourceTypeMedia
                "Font" -> NetworkResourceTypeFont
                "Script" -> NetworkResourceTypeScript
                "TextTrack" -> NetworkResourceTypeTextTrack
                "XHR" -> NetworkResourceTypeXhr
                "Fetch" -> NetworkResourceTypeFetch
                "EventSource" -> NetworkResourceTypeEventSource
                "WebSocket" -> NetworkResourceTypeWebSocket
                "Manifest" -> NetworkResourceTypeManifest
                "SignedExchange" -> NetworkResourceTypeSignedExchange
                "Ping" -> NetworkResourceTypePing
                "CSPViolationReport" -> NetworkResourceTypeCspViolationReport
                "Preflight" -> NetworkResourceTypePreflight
                "Other" -> NetworkResourceTypeOther

instance ToJSON NetworkResourceType where
    toJSON v = A.String $
        case v of
                NetworkResourceTypeDocument -> "Document"
                NetworkResourceTypeStylesheet -> "Stylesheet"
                NetworkResourceTypeImage -> "Image"
                NetworkResourceTypeMedia -> "Media"
                NetworkResourceTypeFont -> "Font"
                NetworkResourceTypeScript -> "Script"
                NetworkResourceTypeTextTrack -> "TextTrack"
                NetworkResourceTypeXhr -> "XHR"
                NetworkResourceTypeFetch -> "Fetch"
                NetworkResourceTypeEventSource -> "EventSource"
                NetworkResourceTypeWebSocket -> "WebSocket"
                NetworkResourceTypeManifest -> "Manifest"
                NetworkResourceTypeSignedExchange -> "SignedExchange"
                NetworkResourceTypePing -> "Ping"
                NetworkResourceTypeCspViolationReport -> "CSPViolationReport"
                NetworkResourceTypePreflight -> "Preflight"
                NetworkResourceTypeOther -> "Other"



type NetworkLoaderId = String

type NetworkRequestId = String

type NetworkInterceptionId = String

data NetworkErrorReason = NetworkErrorReasonFailed | NetworkErrorReasonAborted | NetworkErrorReasonTimedOut | NetworkErrorReasonAccessDenied | NetworkErrorReasonConnectionClosed | NetworkErrorReasonConnectionReset | NetworkErrorReasonConnectionRefused | NetworkErrorReasonConnectionAborted | NetworkErrorReasonConnectionFailed | NetworkErrorReasonNameNotResolved | NetworkErrorReasonInternetDisconnected | NetworkErrorReasonAddressUnreachable | NetworkErrorReasonBlockedByClient | NetworkErrorReasonBlockedByResponse
    deriving Show
instance FromJSON NetworkErrorReason where
    parseJSON = A.withText  "NetworkErrorReason"  $ \v -> 
        pure $ case v of
                "Failed" -> NetworkErrorReasonFailed
                "Aborted" -> NetworkErrorReasonAborted
                "TimedOut" -> NetworkErrorReasonTimedOut
                "AccessDenied" -> NetworkErrorReasonAccessDenied
                "ConnectionClosed" -> NetworkErrorReasonConnectionClosed
                "ConnectionReset" -> NetworkErrorReasonConnectionReset
                "ConnectionRefused" -> NetworkErrorReasonConnectionRefused
                "ConnectionAborted" -> NetworkErrorReasonConnectionAborted
                "ConnectionFailed" -> NetworkErrorReasonConnectionFailed
                "NameNotResolved" -> NetworkErrorReasonNameNotResolved
                "InternetDisconnected" -> NetworkErrorReasonInternetDisconnected
                "AddressUnreachable" -> NetworkErrorReasonAddressUnreachable
                "BlockedByClient" -> NetworkErrorReasonBlockedByClient
                "BlockedByResponse" -> NetworkErrorReasonBlockedByResponse

instance ToJSON NetworkErrorReason where
    toJSON v = A.String $
        case v of
                NetworkErrorReasonFailed -> "Failed"
                NetworkErrorReasonAborted -> "Aborted"
                NetworkErrorReasonTimedOut -> "TimedOut"
                NetworkErrorReasonAccessDenied -> "AccessDenied"
                NetworkErrorReasonConnectionClosed -> "ConnectionClosed"
                NetworkErrorReasonConnectionReset -> "ConnectionReset"
                NetworkErrorReasonConnectionRefused -> "ConnectionRefused"
                NetworkErrorReasonConnectionAborted -> "ConnectionAborted"
                NetworkErrorReasonConnectionFailed -> "ConnectionFailed"
                NetworkErrorReasonNameNotResolved -> "NameNotResolved"
                NetworkErrorReasonInternetDisconnected -> "InternetDisconnected"
                NetworkErrorReasonAddressUnreachable -> "AddressUnreachable"
                NetworkErrorReasonBlockedByClient -> "BlockedByClient"
                NetworkErrorReasonBlockedByResponse -> "BlockedByResponse"



type NetworkTimeSinceEpoch = Int

type NetworkMonotonicTime = Int

type NetworkHeaders = [(String, String)]

data NetworkConnectionType = NetworkConnectionTypeNone | NetworkConnectionTypeCellular2g | NetworkConnectionTypeCellular3g | NetworkConnectionTypeCellular4g | NetworkConnectionTypeBluetooth | NetworkConnectionTypeEthernet | NetworkConnectionTypeWifi | NetworkConnectionTypeWimax | NetworkConnectionTypeOther
    deriving Show
instance FromJSON NetworkConnectionType where
    parseJSON = A.withText  "NetworkConnectionType"  $ \v -> 
        pure $ case v of
                "none" -> NetworkConnectionTypeNone
                "cellular2g" -> NetworkConnectionTypeCellular2g
                "cellular3g" -> NetworkConnectionTypeCellular3g
                "cellular4g" -> NetworkConnectionTypeCellular4g
                "bluetooth" -> NetworkConnectionTypeBluetooth
                "ethernet" -> NetworkConnectionTypeEthernet
                "wifi" -> NetworkConnectionTypeWifi
                "wimax" -> NetworkConnectionTypeWimax
                "other" -> NetworkConnectionTypeOther

instance ToJSON NetworkConnectionType where
    toJSON v = A.String $
        case v of
                NetworkConnectionTypeNone -> "none"
                NetworkConnectionTypeCellular2g -> "cellular2g"
                NetworkConnectionTypeCellular3g -> "cellular3g"
                NetworkConnectionTypeCellular4g -> "cellular4g"
                NetworkConnectionTypeBluetooth -> "bluetooth"
                NetworkConnectionTypeEthernet -> "ethernet"
                NetworkConnectionTypeWifi -> "wifi"
                NetworkConnectionTypeWimax -> "wimax"
                NetworkConnectionTypeOther -> "other"



data NetworkCookieSameSite = NetworkCookieSameSiteStrict | NetworkCookieSameSiteLax | NetworkCookieSameSiteNone
    deriving Show
instance FromJSON NetworkCookieSameSite where
    parseJSON = A.withText  "NetworkCookieSameSite"  $ \v -> 
        pure $ case v of
                "Strict" -> NetworkCookieSameSiteStrict
                "Lax" -> NetworkCookieSameSiteLax
                "None" -> NetworkCookieSameSiteNone

instance ToJSON NetworkCookieSameSite where
    toJSON v = A.String $
        case v of
                NetworkCookieSameSiteStrict -> "Strict"
                NetworkCookieSameSiteLax -> "Lax"
                NetworkCookieSameSiteNone -> "None"



data NetworkResourceTiming = NetworkResourceTiming {
    networkResourceTimingRequestTime :: Int,
    networkResourceTimingProxyStart :: Int,
    networkResourceTimingProxyEnd :: Int,
    networkResourceTimingDnsStart :: Int,
    networkResourceTimingDnsEnd :: Int,
    networkResourceTimingConnectStart :: Int,
    networkResourceTimingConnectEnd :: Int,
    networkResourceTimingSslStart :: Int,
    networkResourceTimingSslEnd :: Int,
    networkResourceTimingSendStart :: Int,
    networkResourceTimingSendEnd :: Int,
    networkResourceTimingReceiveHeadersEnd :: Int
} deriving Show
instance FromJSON  NetworkResourceTiming where
    parseJSON = A.withObject "NetworkResourceTiming" $ \v ->
         NetworkResourceTiming <$> v .:  "requestTime"
            <*> v  .:  "proxyStart"
            <*> v  .:  "proxyEnd"
            <*> v  .:  "dnsStart"
            <*> v  .:  "dnsEnd"
            <*> v  .:  "connectStart"
            <*> v  .:  "connectEnd"
            <*> v  .:  "sslStart"
            <*> v  .:  "sslEnd"
            <*> v  .:  "sendStart"
            <*> v  .:  "sendEnd"
            <*> v  .:  "receiveHeadersEnd"


instance ToJSON NetworkResourceTiming  where
    toJSON v = A.object
        [ "requestTime" .= networkResourceTimingRequestTime v
        , "proxyStart" .= networkResourceTimingProxyStart v
        , "proxyEnd" .= networkResourceTimingProxyEnd v
        , "dnsStart" .= networkResourceTimingDnsStart v
        , "dnsEnd" .= networkResourceTimingDnsEnd v
        , "connectStart" .= networkResourceTimingConnectStart v
        , "connectEnd" .= networkResourceTimingConnectEnd v
        , "sslStart" .= networkResourceTimingSslStart v
        , "sslEnd" .= networkResourceTimingSslEnd v
        , "sendStart" .= networkResourceTimingSendStart v
        , "sendEnd" .= networkResourceTimingSendEnd v
        , "receiveHeadersEnd" .= networkResourceTimingReceiveHeadersEnd v
        ]



data NetworkResourcePriority = NetworkResourcePriorityVeryLow | NetworkResourcePriorityLow | NetworkResourcePriorityMedium | NetworkResourcePriorityHigh | NetworkResourcePriorityVeryHigh
    deriving Show
instance FromJSON NetworkResourcePriority where
    parseJSON = A.withText  "NetworkResourcePriority"  $ \v -> 
        pure $ case v of
                "VeryLow" -> NetworkResourcePriorityVeryLow
                "Low" -> NetworkResourcePriorityLow
                "Medium" -> NetworkResourcePriorityMedium
                "High" -> NetworkResourcePriorityHigh
                "VeryHigh" -> NetworkResourcePriorityVeryHigh

instance ToJSON NetworkResourcePriority where
    toJSON v = A.String $
        case v of
                NetworkResourcePriorityVeryLow -> "VeryLow"
                NetworkResourcePriorityLow -> "Low"
                NetworkResourcePriorityMedium -> "Medium"
                NetworkResourcePriorityHigh -> "High"
                NetworkResourcePriorityVeryHigh -> "VeryHigh"



data NetworkPostDataEntry = NetworkPostDataEntry {
    networkPostDataEntryBytes :: Maybe String
} deriving Show
instance FromJSON  NetworkPostDataEntry where
    parseJSON = A.withObject "NetworkPostDataEntry" $ \v ->
         NetworkPostDataEntry <$> v .:?  "bytes"


instance ToJSON NetworkPostDataEntry  where
    toJSON v = A.object
        [ "bytes" .= networkPostDataEntryBytes v
        ]



data NetworkRequest = NetworkRequest {
    networkRequestUrl :: String,
    networkRequestMethod :: String,
    networkRequestHeaders :: NetworkHeaders,
    networkRequestInitialPriority :: NetworkResourcePriority,
    networkRequestReferrerPolicy :: String,
    networkRequestUrlFragment :: Maybe String,
    networkRequestPostData :: Maybe String,
    networkRequestHasPostData :: Maybe Bool,
    networkRequestMixedContentType :: Maybe SecurityMixedContentType,
    networkRequestIsLinkPreload :: Maybe Bool
} deriving Show
instance FromJSON  NetworkRequest where
    parseJSON = A.withObject "NetworkRequest" $ \v ->
         NetworkRequest <$> v .:  "url"
            <*> v  .:  "method"
            <*> v  .:  "headers"
            <*> v  .:  "initialPriority"
            <*> v  .:  "referrerPolicy"
            <*> v  .:?  "urlFragment"
            <*> v  .:?  "postData"
            <*> v  .:?  "hasPostData"
            <*> v  .:?  "mixedContentType"
            <*> v  .:?  "isLinkPreload"


instance ToJSON NetworkRequest  where
    toJSON v = A.object
        [ "url" .= networkRequestUrl v
        , "method" .= networkRequestMethod v
        , "headers" .= networkRequestHeaders v
        , "initialPriority" .= networkRequestInitialPriority v
        , "referrerPolicy" .= networkRequestReferrerPolicy v
        , "urlFragment" .= networkRequestUrlFragment v
        , "postData" .= networkRequestPostData v
        , "hasPostData" .= networkRequestHasPostData v
        , "mixedContentType" .= networkRequestMixedContentType v
        , "isLinkPreload" .= networkRequestIsLinkPreload v
        ]



data NetworkSignedCertificateTimestamp = NetworkSignedCertificateTimestamp {
    networkSignedCertificateTimestampStatus :: String,
    networkSignedCertificateTimestampOrigin :: String,
    networkSignedCertificateTimestampLogDescription :: String,
    networkSignedCertificateTimestampLogId :: String,
    networkSignedCertificateTimestampTimestamp :: Int,
    networkSignedCertificateTimestampHashAlgorithm :: String,
    networkSignedCertificateTimestampSignatureAlgorithm :: String,
    networkSignedCertificateTimestampSignatureData :: String
} deriving Show
instance FromJSON  NetworkSignedCertificateTimestamp where
    parseJSON = A.withObject "NetworkSignedCertificateTimestamp" $ \v ->
         NetworkSignedCertificateTimestamp <$> v .:  "status"
            <*> v  .:  "origin"
            <*> v  .:  "logDescription"
            <*> v  .:  "logId"
            <*> v  .:  "timestamp"
            <*> v  .:  "hashAlgorithm"
            <*> v  .:  "signatureAlgorithm"
            <*> v  .:  "signatureData"


instance ToJSON NetworkSignedCertificateTimestamp  where
    toJSON v = A.object
        [ "status" .= networkSignedCertificateTimestampStatus v
        , "origin" .= networkSignedCertificateTimestampOrigin v
        , "logDescription" .= networkSignedCertificateTimestampLogDescription v
        , "logId" .= networkSignedCertificateTimestampLogId v
        , "timestamp" .= networkSignedCertificateTimestampTimestamp v
        , "hashAlgorithm" .= networkSignedCertificateTimestampHashAlgorithm v
        , "signatureAlgorithm" .= networkSignedCertificateTimestampSignatureAlgorithm v
        , "signatureData" .= networkSignedCertificateTimestampSignatureData v
        ]



data NetworkSecurityDetails = NetworkSecurityDetails {
    networkSecurityDetailsProtocol :: String,
    networkSecurityDetailsKeyExchange :: String,
    networkSecurityDetailsCipher :: String,
    networkSecurityDetailsCertificateId :: SecurityCertificateId,
    networkSecurityDetailsSubjectName :: String,
    networkSecurityDetailsSanList :: [String],
    networkSecurityDetailsIssuer :: String,
    networkSecurityDetailsValidFrom :: NetworkTimeSinceEpoch,
    networkSecurityDetailsValidTo :: NetworkTimeSinceEpoch,
    networkSecurityDetailsSignedCertificateTimestampList :: [NetworkSignedCertificateTimestamp],
    networkSecurityDetailsCertificateTransparencyCompliance :: NetworkCertificateTransparencyCompliance,
    networkSecurityDetailsKeyExchangeGroup :: Maybe String,
    networkSecurityDetailsMac :: Maybe String
} deriving Show
instance FromJSON  NetworkSecurityDetails where
    parseJSON = A.withObject "NetworkSecurityDetails" $ \v ->
         NetworkSecurityDetails <$> v .:  "protocol"
            <*> v  .:  "keyExchange"
            <*> v  .:  "cipher"
            <*> v  .:  "certificateId"
            <*> v  .:  "subjectName"
            <*> v  .:  "sanList"
            <*> v  .:  "issuer"
            <*> v  .:  "validFrom"
            <*> v  .:  "validTo"
            <*> v  .:  "signedCertificateTimestampList"
            <*> v  .:  "certificateTransparencyCompliance"
            <*> v  .:?  "keyExchangeGroup"
            <*> v  .:?  "mac"


instance ToJSON NetworkSecurityDetails  where
    toJSON v = A.object
        [ "protocol" .= networkSecurityDetailsProtocol v
        , "keyExchange" .= networkSecurityDetailsKeyExchange v
        , "cipher" .= networkSecurityDetailsCipher v
        , "certificateId" .= networkSecurityDetailsCertificateId v
        , "subjectName" .= networkSecurityDetailsSubjectName v
        , "sanList" .= networkSecurityDetailsSanList v
        , "issuer" .= networkSecurityDetailsIssuer v
        , "validFrom" .= networkSecurityDetailsValidFrom v
        , "validTo" .= networkSecurityDetailsValidTo v
        , "signedCertificateTimestampList" .= networkSecurityDetailsSignedCertificateTimestampList v
        , "certificateTransparencyCompliance" .= networkSecurityDetailsCertificateTransparencyCompliance v
        , "keyExchangeGroup" .= networkSecurityDetailsKeyExchangeGroup v
        , "mac" .= networkSecurityDetailsMac v
        ]



data NetworkCertificateTransparencyCompliance = NetworkCertificateTransparencyComplianceUnknown | NetworkCertificateTransparencyComplianceNotCompliant | NetworkCertificateTransparencyComplianceCompliant
    deriving Show
instance FromJSON NetworkCertificateTransparencyCompliance where
    parseJSON = A.withText  "NetworkCertificateTransparencyCompliance"  $ \v -> 
        pure $ case v of
                "unknown" -> NetworkCertificateTransparencyComplianceUnknown
                "not-compliant" -> NetworkCertificateTransparencyComplianceNotCompliant
                "compliant" -> NetworkCertificateTransparencyComplianceCompliant

instance ToJSON NetworkCertificateTransparencyCompliance where
    toJSON v = A.String $
        case v of
                NetworkCertificateTransparencyComplianceUnknown -> "unknown"
                NetworkCertificateTransparencyComplianceNotCompliant -> "not-compliant"
                NetworkCertificateTransparencyComplianceCompliant -> "compliant"



data NetworkBlockedReason = NetworkBlockedReasonOther | NetworkBlockedReasonCsp | NetworkBlockedReasonMixedContent | NetworkBlockedReasonOrigin | NetworkBlockedReasonInspector | NetworkBlockedReasonSubresourceFilter | NetworkBlockedReasonContentType | NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader | NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage | NetworkBlockedReasonCorpNotSameOrigin | NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep | NetworkBlockedReasonCorpNotSameSite
    deriving Show
instance FromJSON NetworkBlockedReason where
    parseJSON = A.withText  "NetworkBlockedReason"  $ \v -> 
        pure $ case v of
                "other" -> NetworkBlockedReasonOther
                "csp" -> NetworkBlockedReasonCsp
                "mixed-content" -> NetworkBlockedReasonMixedContent
                "origin" -> NetworkBlockedReasonOrigin
                "inspector" -> NetworkBlockedReasonInspector
                "subresource-filter" -> NetworkBlockedReasonSubresourceFilter
                "content-type" -> NetworkBlockedReasonContentType
                "coep-frame-resource-needs-coep-header" -> NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader
                "coop-sandboxed-iframe-cannot-navigate-to-coop-page" -> NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage
                "corp-not-same-origin" -> NetworkBlockedReasonCorpNotSameOrigin
                "corp-not-same-origin-after-defaulted-to-same-origin-by-coep" -> NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep
                "corp-not-same-site" -> NetworkBlockedReasonCorpNotSameSite

instance ToJSON NetworkBlockedReason where
    toJSON v = A.String $
        case v of
                NetworkBlockedReasonOther -> "other"
                NetworkBlockedReasonCsp -> "csp"
                NetworkBlockedReasonMixedContent -> "mixed-content"
                NetworkBlockedReasonOrigin -> "origin"
                NetworkBlockedReasonInspector -> "inspector"
                NetworkBlockedReasonSubresourceFilter -> "subresource-filter"
                NetworkBlockedReasonContentType -> "content-type"
                NetworkBlockedReasonCoepFrameResourceNeedsCoepHeader -> "coep-frame-resource-needs-coep-header"
                NetworkBlockedReasonCoopSandboxedIframeCannotNavigateToCoopPage -> "coop-sandboxed-iframe-cannot-navigate-to-coop-page"
                NetworkBlockedReasonCorpNotSameOrigin -> "corp-not-same-origin"
                NetworkBlockedReasonCorpNotSameOriginAfterDefaultedToSameOriginByCoep -> "corp-not-same-origin-after-defaulted-to-same-origin-by-coep"
                NetworkBlockedReasonCorpNotSameSite -> "corp-not-same-site"



data NetworkCorsError = NetworkCorsErrorDisallowedByMode | NetworkCorsErrorInvalidResponse | NetworkCorsErrorWildcardOriginNotAllowed | NetworkCorsErrorMissingAllowOriginHeader | NetworkCorsErrorMultipleAllowOriginValues | NetworkCorsErrorInvalidAllowOriginValue | NetworkCorsErrorAllowOriginMismatch | NetworkCorsErrorInvalidAllowCredentials | NetworkCorsErrorCorsDisabledScheme | NetworkCorsErrorPreflightInvalidStatus | NetworkCorsErrorPreflightDisallowedRedirect | NetworkCorsErrorPreflightWildcardOriginNotAllowed | NetworkCorsErrorPreflightMissingAllowOriginHeader | NetworkCorsErrorPreflightMultipleAllowOriginValues | NetworkCorsErrorPreflightInvalidAllowOriginValue | NetworkCorsErrorPreflightAllowOriginMismatch | NetworkCorsErrorPreflightInvalidAllowCredentials | NetworkCorsErrorPreflightMissingAllowExternal | NetworkCorsErrorPreflightInvalidAllowExternal | NetworkCorsErrorPreflightMissingAllowPrivateNetwork | NetworkCorsErrorPreflightInvalidAllowPrivateNetwork | NetworkCorsErrorInvalidAllowMethodsPreflightResponse | NetworkCorsErrorInvalidAllowHeadersPreflightResponse | NetworkCorsErrorMethodDisallowedByPreflightResponse | NetworkCorsErrorHeaderDisallowedByPreflightResponse | NetworkCorsErrorRedirectContainsCredentials | NetworkCorsErrorInsecurePrivateNetwork | NetworkCorsErrorInvalidPrivateNetworkAccess | NetworkCorsErrorUnexpectedPrivateNetworkAccess | NetworkCorsErrorNoCorsRedirectModeNotFollow
    deriving Show
instance FromJSON NetworkCorsError where
    parseJSON = A.withText  "NetworkCorsError"  $ \v -> 
        pure $ case v of
                "DisallowedByMode" -> NetworkCorsErrorDisallowedByMode
                "InvalidResponse" -> NetworkCorsErrorInvalidResponse
                "WildcardOriginNotAllowed" -> NetworkCorsErrorWildcardOriginNotAllowed
                "MissingAllowOriginHeader" -> NetworkCorsErrorMissingAllowOriginHeader
                "MultipleAllowOriginValues" -> NetworkCorsErrorMultipleAllowOriginValues
                "InvalidAllowOriginValue" -> NetworkCorsErrorInvalidAllowOriginValue
                "AllowOriginMismatch" -> NetworkCorsErrorAllowOriginMismatch
                "InvalidAllowCredentials" -> NetworkCorsErrorInvalidAllowCredentials
                "CorsDisabledScheme" -> NetworkCorsErrorCorsDisabledScheme
                "PreflightInvalidStatus" -> NetworkCorsErrorPreflightInvalidStatus
                "PreflightDisallowedRedirect" -> NetworkCorsErrorPreflightDisallowedRedirect
                "PreflightWildcardOriginNotAllowed" -> NetworkCorsErrorPreflightWildcardOriginNotAllowed
                "PreflightMissingAllowOriginHeader" -> NetworkCorsErrorPreflightMissingAllowOriginHeader
                "PreflightMultipleAllowOriginValues" -> NetworkCorsErrorPreflightMultipleAllowOriginValues
                "PreflightInvalidAllowOriginValue" -> NetworkCorsErrorPreflightInvalidAllowOriginValue
                "PreflightAllowOriginMismatch" -> NetworkCorsErrorPreflightAllowOriginMismatch
                "PreflightInvalidAllowCredentials" -> NetworkCorsErrorPreflightInvalidAllowCredentials
                "PreflightMissingAllowExternal" -> NetworkCorsErrorPreflightMissingAllowExternal
                "PreflightInvalidAllowExternal" -> NetworkCorsErrorPreflightInvalidAllowExternal
                "PreflightMissingAllowPrivateNetwork" -> NetworkCorsErrorPreflightMissingAllowPrivateNetwork
                "PreflightInvalidAllowPrivateNetwork" -> NetworkCorsErrorPreflightInvalidAllowPrivateNetwork
                "InvalidAllowMethodsPreflightResponse" -> NetworkCorsErrorInvalidAllowMethodsPreflightResponse
                "InvalidAllowHeadersPreflightResponse" -> NetworkCorsErrorInvalidAllowHeadersPreflightResponse
                "MethodDisallowedByPreflightResponse" -> NetworkCorsErrorMethodDisallowedByPreflightResponse
                "HeaderDisallowedByPreflightResponse" -> NetworkCorsErrorHeaderDisallowedByPreflightResponse
                "RedirectContainsCredentials" -> NetworkCorsErrorRedirectContainsCredentials
                "InsecurePrivateNetwork" -> NetworkCorsErrorInsecurePrivateNetwork
                "InvalidPrivateNetworkAccess" -> NetworkCorsErrorInvalidPrivateNetworkAccess
                "UnexpectedPrivateNetworkAccess" -> NetworkCorsErrorUnexpectedPrivateNetworkAccess
                "NoCorsRedirectModeNotFollow" -> NetworkCorsErrorNoCorsRedirectModeNotFollow

instance ToJSON NetworkCorsError where
    toJSON v = A.String $
        case v of
                NetworkCorsErrorDisallowedByMode -> "DisallowedByMode"
                NetworkCorsErrorInvalidResponse -> "InvalidResponse"
                NetworkCorsErrorWildcardOriginNotAllowed -> "WildcardOriginNotAllowed"
                NetworkCorsErrorMissingAllowOriginHeader -> "MissingAllowOriginHeader"
                NetworkCorsErrorMultipleAllowOriginValues -> "MultipleAllowOriginValues"
                NetworkCorsErrorInvalidAllowOriginValue -> "InvalidAllowOriginValue"
                NetworkCorsErrorAllowOriginMismatch -> "AllowOriginMismatch"
                NetworkCorsErrorInvalidAllowCredentials -> "InvalidAllowCredentials"
                NetworkCorsErrorCorsDisabledScheme -> "CorsDisabledScheme"
                NetworkCorsErrorPreflightInvalidStatus -> "PreflightInvalidStatus"
                NetworkCorsErrorPreflightDisallowedRedirect -> "PreflightDisallowedRedirect"
                NetworkCorsErrorPreflightWildcardOriginNotAllowed -> "PreflightWildcardOriginNotAllowed"
                NetworkCorsErrorPreflightMissingAllowOriginHeader -> "PreflightMissingAllowOriginHeader"
                NetworkCorsErrorPreflightMultipleAllowOriginValues -> "PreflightMultipleAllowOriginValues"
                NetworkCorsErrorPreflightInvalidAllowOriginValue -> "PreflightInvalidAllowOriginValue"
                NetworkCorsErrorPreflightAllowOriginMismatch -> "PreflightAllowOriginMismatch"
                NetworkCorsErrorPreflightInvalidAllowCredentials -> "PreflightInvalidAllowCredentials"
                NetworkCorsErrorPreflightMissingAllowExternal -> "PreflightMissingAllowExternal"
                NetworkCorsErrorPreflightInvalidAllowExternal -> "PreflightInvalidAllowExternal"
                NetworkCorsErrorPreflightMissingAllowPrivateNetwork -> "PreflightMissingAllowPrivateNetwork"
                NetworkCorsErrorPreflightInvalidAllowPrivateNetwork -> "PreflightInvalidAllowPrivateNetwork"
                NetworkCorsErrorInvalidAllowMethodsPreflightResponse -> "InvalidAllowMethodsPreflightResponse"
                NetworkCorsErrorInvalidAllowHeadersPreflightResponse -> "InvalidAllowHeadersPreflightResponse"
                NetworkCorsErrorMethodDisallowedByPreflightResponse -> "MethodDisallowedByPreflightResponse"
                NetworkCorsErrorHeaderDisallowedByPreflightResponse -> "HeaderDisallowedByPreflightResponse"
                NetworkCorsErrorRedirectContainsCredentials -> "RedirectContainsCredentials"
                NetworkCorsErrorInsecurePrivateNetwork -> "InsecurePrivateNetwork"
                NetworkCorsErrorInvalidPrivateNetworkAccess -> "InvalidPrivateNetworkAccess"
                NetworkCorsErrorUnexpectedPrivateNetworkAccess -> "UnexpectedPrivateNetworkAccess"
                NetworkCorsErrorNoCorsRedirectModeNotFollow -> "NoCorsRedirectModeNotFollow"



data NetworkCorsErrorStatus = NetworkCorsErrorStatus {
    networkCorsErrorStatusCorsError :: NetworkCorsError,
    networkCorsErrorStatusFailedParameter :: String
} deriving Show
instance FromJSON  NetworkCorsErrorStatus where
    parseJSON = A.withObject "NetworkCorsErrorStatus" $ \v ->
         NetworkCorsErrorStatus <$> v .:  "corsError"
            <*> v  .:  "failedParameter"


instance ToJSON NetworkCorsErrorStatus  where
    toJSON v = A.object
        [ "corsError" .= networkCorsErrorStatusCorsError v
        , "failedParameter" .= networkCorsErrorStatusFailedParameter v
        ]



data NetworkServiceWorkerResponseSource = NetworkServiceWorkerResponseSourceCacheStorage | NetworkServiceWorkerResponseSourceHttpCache | NetworkServiceWorkerResponseSourceFallbackCode | NetworkServiceWorkerResponseSourceNetwork
    deriving Show
instance FromJSON NetworkServiceWorkerResponseSource where
    parseJSON = A.withText  "NetworkServiceWorkerResponseSource"  $ \v -> 
        pure $ case v of
                "cache-storage" -> NetworkServiceWorkerResponseSourceCacheStorage
                "http-cache" -> NetworkServiceWorkerResponseSourceHttpCache
                "fallback-code" -> NetworkServiceWorkerResponseSourceFallbackCode
                "network" -> NetworkServiceWorkerResponseSourceNetwork

instance ToJSON NetworkServiceWorkerResponseSource where
    toJSON v = A.String $
        case v of
                NetworkServiceWorkerResponseSourceCacheStorage -> "cache-storage"
                NetworkServiceWorkerResponseSourceHttpCache -> "http-cache"
                NetworkServiceWorkerResponseSourceFallbackCode -> "fallback-code"
                NetworkServiceWorkerResponseSourceNetwork -> "network"



data NetworkResponse = NetworkResponse {
    networkResponseUrl :: String,
    networkResponseStatus :: Int,
    networkResponseStatusText :: String,
    networkResponseHeaders :: NetworkHeaders,
    networkResponseMimeType :: String,
    networkResponseConnectionReused :: Bool,
    networkResponseConnectionId :: Int,
    networkResponseEncodedDataLength :: Int,
    networkResponseSecurityState :: SecuritySecurityState,
    networkResponseRequestHeaders :: Maybe NetworkHeaders,
    networkResponseRemoteIpAddress :: Maybe String,
    networkResponseRemotePort :: Maybe Int,
    networkResponseFromDiskCache :: Maybe Bool,
    networkResponseFromServiceWorker :: Maybe Bool,
    networkResponseFromPrefetchCache :: Maybe Bool,
    networkResponseTiming :: Maybe NetworkResourceTiming,
    networkResponseServiceWorkerResponseSource :: Maybe NetworkServiceWorkerResponseSource,
    networkResponseResponseTime :: Maybe NetworkTimeSinceEpoch,
    networkResponseCacheStorageCacheName :: Maybe String,
    networkResponseProtocol :: Maybe String,
    networkResponseSecurityDetails :: Maybe NetworkSecurityDetails
} deriving Show
instance FromJSON  NetworkResponse where
    parseJSON = A.withObject "NetworkResponse" $ \v ->
         NetworkResponse <$> v .:  "url"
            <*> v  .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:  "mimeType"
            <*> v  .:  "connectionReused"
            <*> v  .:  "connectionId"
            <*> v  .:  "encodedDataLength"
            <*> v  .:  "securityState"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "remoteIPAddress"
            <*> v  .:?  "remotePort"
            <*> v  .:?  "fromDiskCache"
            <*> v  .:?  "fromServiceWorker"
            <*> v  .:?  "fromPrefetchCache"
            <*> v  .:?  "timing"
            <*> v  .:?  "serviceWorkerResponseSource"
            <*> v  .:?  "responseTime"
            <*> v  .:?  "cacheStorageCacheName"
            <*> v  .:?  "protocol"
            <*> v  .:?  "securityDetails"


instance ToJSON NetworkResponse  where
    toJSON v = A.object
        [ "url" .= networkResponseUrl v
        , "status" .= networkResponseStatus v
        , "statusText" .= networkResponseStatusText v
        , "headers" .= networkResponseHeaders v
        , "mimeType" .= networkResponseMimeType v
        , "connectionReused" .= networkResponseConnectionReused v
        , "connectionId" .= networkResponseConnectionId v
        , "encodedDataLength" .= networkResponseEncodedDataLength v
        , "securityState" .= networkResponseSecurityState v
        , "requestHeaders" .= networkResponseRequestHeaders v
        , "remoteIPAddress" .= networkResponseRemoteIpAddress v
        , "remotePort" .= networkResponseRemotePort v
        , "fromDiskCache" .= networkResponseFromDiskCache v
        , "fromServiceWorker" .= networkResponseFromServiceWorker v
        , "fromPrefetchCache" .= networkResponseFromPrefetchCache v
        , "timing" .= networkResponseTiming v
        , "serviceWorkerResponseSource" .= networkResponseServiceWorkerResponseSource v
        , "responseTime" .= networkResponseResponseTime v
        , "cacheStorageCacheName" .= networkResponseCacheStorageCacheName v
        , "protocol" .= networkResponseProtocol v
        , "securityDetails" .= networkResponseSecurityDetails v
        ]



data NetworkWebSocketRequest = NetworkWebSocketRequest {
    networkWebSocketRequestHeaders :: NetworkHeaders
} deriving Show
instance FromJSON  NetworkWebSocketRequest where
    parseJSON = A.withObject "NetworkWebSocketRequest" $ \v ->
         NetworkWebSocketRequest <$> v .:  "headers"


instance ToJSON NetworkWebSocketRequest  where
    toJSON v = A.object
        [ "headers" .= networkWebSocketRequestHeaders v
        ]



data NetworkWebSocketResponse = NetworkWebSocketResponse {
    networkWebSocketResponseStatus :: Int,
    networkWebSocketResponseStatusText :: String,
    networkWebSocketResponseHeaders :: NetworkHeaders,
    networkWebSocketResponseHeadersText :: Maybe String,
    networkWebSocketResponseRequestHeaders :: Maybe NetworkHeaders,
    networkWebSocketResponseRequestHeadersText :: Maybe String
} deriving Show
instance FromJSON  NetworkWebSocketResponse where
    parseJSON = A.withObject "NetworkWebSocketResponse" $ \v ->
         NetworkWebSocketResponse <$> v .:  "status"
            <*> v  .:  "statusText"
            <*> v  .:  "headers"
            <*> v  .:?  "headersText"
            <*> v  .:?  "requestHeaders"
            <*> v  .:?  "requestHeadersText"


instance ToJSON NetworkWebSocketResponse  where
    toJSON v = A.object
        [ "status" .= networkWebSocketResponseStatus v
        , "statusText" .= networkWebSocketResponseStatusText v
        , "headers" .= networkWebSocketResponseHeaders v
        , "headersText" .= networkWebSocketResponseHeadersText v
        , "requestHeaders" .= networkWebSocketResponseRequestHeaders v
        , "requestHeadersText" .= networkWebSocketResponseRequestHeadersText v
        ]



data NetworkWebSocketFrame = NetworkWebSocketFrame {
    networkWebSocketFrameOpcode :: Int,
    networkWebSocketFrameMask :: Bool,
    networkWebSocketFramePayloadData :: String
} deriving Show
instance FromJSON  NetworkWebSocketFrame where
    parseJSON = A.withObject "NetworkWebSocketFrame" $ \v ->
         NetworkWebSocketFrame <$> v .:  "opcode"
            <*> v  .:  "mask"
            <*> v  .:  "payloadData"


instance ToJSON NetworkWebSocketFrame  where
    toJSON v = A.object
        [ "opcode" .= networkWebSocketFrameOpcode v
        , "mask" .= networkWebSocketFrameMask v
        , "payloadData" .= networkWebSocketFramePayloadData v
        ]



data NetworkCachedResource = NetworkCachedResource {
    networkCachedResourceUrl :: String,
    networkCachedResourceType :: NetworkResourceType,
    networkCachedResourceBodySize :: Int,
    networkCachedResourceResponse :: Maybe NetworkResponse
} deriving Show
instance FromJSON  NetworkCachedResource where
    parseJSON = A.withObject "NetworkCachedResource" $ \v ->
         NetworkCachedResource <$> v .:  "url"
            <*> v  .:  "type"
            <*> v  .:  "bodySize"
            <*> v  .:?  "response"


instance ToJSON NetworkCachedResource  where
    toJSON v = A.object
        [ "url" .= networkCachedResourceUrl v
        , "type" .= networkCachedResourceType v
        , "bodySize" .= networkCachedResourceBodySize v
        , "response" .= networkCachedResourceResponse v
        ]



data NetworkInitiator = NetworkInitiator {
    networkInitiatorType :: String,
    networkInitiatorStack :: Maybe RuntimeStackTrace,
    networkInitiatorUrl :: Maybe String,
    networkInitiatorLineNumber :: Maybe Int,
    networkInitiatorColumnNumber :: Maybe Int,
    networkInitiatorRequestId :: Maybe NetworkRequestId
} deriving Show
instance FromJSON  NetworkInitiator where
    parseJSON = A.withObject "NetworkInitiator" $ \v ->
         NetworkInitiator <$> v .:  "type"
            <*> v  .:?  "stack"
            <*> v  .:?  "url"
            <*> v  .:?  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "requestId"


instance ToJSON NetworkInitiator  where
    toJSON v = A.object
        [ "type" .= networkInitiatorType v
        , "stack" .= networkInitiatorStack v
        , "url" .= networkInitiatorUrl v
        , "lineNumber" .= networkInitiatorLineNumber v
        , "columnNumber" .= networkInitiatorColumnNumber v
        , "requestId" .= networkInitiatorRequestId v
        ]



data NetworkCookie = NetworkCookie {
    networkCookieName :: String,
    networkCookieValue :: String,
    networkCookieDomain :: String,
    networkCookiePath :: String,
    networkCookieExpires :: Int,
    networkCookieSize :: Int,
    networkCookieHttpOnly :: Bool,
    networkCookieSecure :: Bool,
    networkCookieSession :: Bool,
    networkCookieSameSite :: Maybe NetworkCookieSameSite
} deriving Show
instance FromJSON  NetworkCookie where
    parseJSON = A.withObject "NetworkCookie" $ \v ->
         NetworkCookie <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:  "domain"
            <*> v  .:  "path"
            <*> v  .:  "expires"
            <*> v  .:  "size"
            <*> v  .:  "httpOnly"
            <*> v  .:  "secure"
            <*> v  .:  "session"
            <*> v  .:?  "sameSite"


instance ToJSON NetworkCookie  where
    toJSON v = A.object
        [ "name" .= networkCookieName v
        , "value" .= networkCookieValue v
        , "domain" .= networkCookieDomain v
        , "path" .= networkCookiePath v
        , "expires" .= networkCookieExpires v
        , "size" .= networkCookieSize v
        , "httpOnly" .= networkCookieHttpOnly v
        , "secure" .= networkCookieSecure v
        , "session" .= networkCookieSession v
        , "sameSite" .= networkCookieSameSite v
        ]



data NetworkCookieParam = NetworkCookieParam {
    networkCookieParamName :: String,
    networkCookieParamValue :: String,
    networkCookieParamUrl :: Maybe String,
    networkCookieParamDomain :: Maybe String,
    networkCookieParamPath :: Maybe String,
    networkCookieParamSecure :: Maybe Bool,
    networkCookieParamHttpOnly :: Maybe Bool,
    networkCookieParamSameSite :: Maybe NetworkCookieSameSite,
    networkCookieParamExpires :: Maybe NetworkTimeSinceEpoch
} deriving Show
instance FromJSON  NetworkCookieParam where
    parseJSON = A.withObject "NetworkCookieParam" $ \v ->
         NetworkCookieParam <$> v .:  "name"
            <*> v  .:  "value"
            <*> v  .:?  "url"
            <*> v  .:?  "domain"
            <*> v  .:?  "path"
            <*> v  .:?  "secure"
            <*> v  .:?  "httpOnly"
            <*> v  .:?  "sameSite"
            <*> v  .:?  "expires"


instance ToJSON NetworkCookieParam  where
    toJSON v = A.object
        [ "name" .= networkCookieParamName v
        , "value" .= networkCookieParamValue v
        , "url" .= networkCookieParamUrl v
        , "domain" .= networkCookieParamDomain v
        , "path" .= networkCookieParamPath v
        , "secure" .= networkCookieParamSecure v
        , "httpOnly" .= networkCookieParamHttpOnly v
        , "sameSite" .= networkCookieParamSameSite v
        , "expires" .= networkCookieParamExpires v
        ]



networkClearBrowserCache :: Session -> IO (Maybe Error)
networkClearBrowserCache session  = sendReceiveCommand (conn session) ("Network","clearBrowserCache") ([] ++ (catMaybes []))


networkClearBrowserCookies :: Session -> IO (Maybe Error)
networkClearBrowserCookies session  = sendReceiveCommand (conn session) ("Network","clearBrowserCookies") ([] ++ (catMaybes []))


networkDeleteCookies :: Session -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
networkDeleteCookies session networkDeleteCookiesName networkDeleteCookiesUrl networkDeleteCookiesDomain networkDeleteCookiesPath = sendReceiveCommand (conn session) ("Network","deleteCookies") ([("networkDeleteCookiesName", ToJSONEx networkDeleteCookiesName)] ++ (catMaybes [fmap (("networkDeleteCookiesUrl",) . ToJSONEx) networkDeleteCookiesUrl, fmap (("networkDeleteCookiesDomain",) . ToJSONEx) networkDeleteCookiesDomain, fmap (("networkDeleteCookiesPath",) . ToJSONEx) networkDeleteCookiesPath]))


networkDisable :: Session -> IO (Maybe Error)
networkDisable session  = sendReceiveCommand (conn session) ("Network","disable") ([] ++ (catMaybes []))


networkEmulateNetworkConditions :: Session -> Bool -> Int -> Int -> Int -> Maybe NetworkConnectionType -> IO (Maybe Error)
networkEmulateNetworkConditions session networkEmulateNetworkConditionsOffline networkEmulateNetworkConditionsLatency networkEmulateNetworkConditionsDownloadThroughput networkEmulateNetworkConditionsUploadThroughput networkEmulateNetworkConditionsConnectionType = sendReceiveCommand (conn session) ("Network","emulateNetworkConditions") ([("networkEmulateNetworkConditionsOffline", ToJSONEx networkEmulateNetworkConditionsOffline), ("networkEmulateNetworkConditionsLatency", ToJSONEx networkEmulateNetworkConditionsLatency), ("networkEmulateNetworkConditionsDownloadThroughput", ToJSONEx networkEmulateNetworkConditionsDownloadThroughput), ("networkEmulateNetworkConditionsUploadThroughput", ToJSONEx networkEmulateNetworkConditionsUploadThroughput)] ++ (catMaybes [fmap (("networkEmulateNetworkConditionsConnectionType",) . ToJSONEx) networkEmulateNetworkConditionsConnectionType]))


networkEnable :: Session -> Maybe Int -> IO (Maybe Error)
networkEnable session networkEnableMaxPostDataSize = sendReceiveCommand (conn session) ("Network","enable") ([] ++ (catMaybes [fmap (("networkEnableMaxPostDataSize",) . ToJSONEx) networkEnableMaxPostDataSize]))

data NetworkGetAllCookies = NetworkGetAllCookies {
    networkGetAllCookiesCookies :: [NetworkCookie]
} deriving Show
instance FromJSON  NetworkGetAllCookies where
    parseJSON = A.withObject "NetworkGetAllCookies" $ \v ->
         NetworkGetAllCookies <$> v .:  "cookies"



networkGetAllCookies :: Session -> IO (Either Error NetworkGetAllCookies)
networkGetAllCookies session  = sendReceiveCommandResult (conn session) ("Network","getAllCookies") ([] ++ (catMaybes []))

data NetworkGetCookies = NetworkGetCookies {
    networkGetCookiesCookies :: [NetworkCookie]
} deriving Show
instance FromJSON  NetworkGetCookies where
    parseJSON = A.withObject "NetworkGetCookies" $ \v ->
         NetworkGetCookies <$> v .:  "cookies"



networkGetCookies :: Session -> Maybe [String] -> IO (Either Error NetworkGetCookies)
networkGetCookies session networkGetCookiesUrls = sendReceiveCommandResult (conn session) ("Network","getCookies") ([] ++ (catMaybes [fmap (("networkGetCookiesUrls",) . ToJSONEx) networkGetCookiesUrls]))

data NetworkGetResponseBody = NetworkGetResponseBody {
    networkGetResponseBodyBody :: String,
    networkGetResponseBodyBase64Encoded :: Bool
} deriving Show
instance FromJSON  NetworkGetResponseBody where
    parseJSON = A.withObject "NetworkGetResponseBody" $ \v ->
         NetworkGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



networkGetResponseBody :: Session -> NetworkRequestId -> IO (Either Error NetworkGetResponseBody)
networkGetResponseBody session networkGetResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Network","getResponseBody") ([("networkGetResponseBodyRequestId", ToJSONEx networkGetResponseBodyRequestId)] ++ (catMaybes []))

data NetworkGetRequestPostData = NetworkGetRequestPostData {
    networkGetRequestPostDataPostData :: String
} deriving Show
instance FromJSON  NetworkGetRequestPostData where
    parseJSON = A.withObject "NetworkGetRequestPostData" $ \v ->
         NetworkGetRequestPostData <$> v .:  "postData"



networkGetRequestPostData :: Session -> NetworkRequestId -> IO (Either Error NetworkGetRequestPostData)
networkGetRequestPostData session networkGetRequestPostDataRequestId = sendReceiveCommandResult (conn session) ("Network","getRequestPostData") ([("networkGetRequestPostDataRequestId", ToJSONEx networkGetRequestPostDataRequestId)] ++ (catMaybes []))


networkSetCacheDisabled :: Session -> Bool -> IO (Maybe Error)
networkSetCacheDisabled session networkSetCacheDisabledCacheDisabled = sendReceiveCommand (conn session) ("Network","setCacheDisabled") ([("networkSetCacheDisabledCacheDisabled", ToJSONEx networkSetCacheDisabledCacheDisabled)] ++ (catMaybes []))


networkSetCookie :: Session -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe NetworkCookieSameSite -> Maybe NetworkTimeSinceEpoch -> IO (Maybe Error)
networkSetCookie session networkSetCookieName networkSetCookieValue networkSetCookieUrl networkSetCookieDomain networkSetCookiePath networkSetCookieSecure networkSetCookieHttpOnly networkSetCookieSameSite networkSetCookieExpires = sendReceiveCommand (conn session) ("Network","setCookie") ([("networkSetCookieName", ToJSONEx networkSetCookieName), ("networkSetCookieValue", ToJSONEx networkSetCookieValue)] ++ (catMaybes [fmap (("networkSetCookieUrl",) . ToJSONEx) networkSetCookieUrl, fmap (("networkSetCookieDomain",) . ToJSONEx) networkSetCookieDomain, fmap (("networkSetCookiePath",) . ToJSONEx) networkSetCookiePath, fmap (("networkSetCookieSecure",) . ToJSONEx) networkSetCookieSecure, fmap (("networkSetCookieHttpOnly",) . ToJSONEx) networkSetCookieHttpOnly, fmap (("networkSetCookieSameSite",) . ToJSONEx) networkSetCookieSameSite, fmap (("networkSetCookieExpires",) . ToJSONEx) networkSetCookieExpires]))


networkSetCookies :: Session -> [NetworkCookieParam] -> IO (Maybe Error)
networkSetCookies session networkSetCookiesCookies = sendReceiveCommand (conn session) ("Network","setCookies") ([("networkSetCookiesCookies", ToJSONEx networkSetCookiesCookies)] ++ (catMaybes []))


networkSetExtraHTTPHeaders :: Session -> NetworkHeaders -> IO (Maybe Error)
networkSetExtraHTTPHeaders session networkSetExtraHTTPHeadersHeaders = sendReceiveCommand (conn session) ("Network","setExtraHTTPHeaders") ([("networkSetExtraHTTPHeadersHeaders", ToJSONEx networkSetExtraHTTPHeadersHeaders)] ++ (catMaybes []))


networkSetUserAgentOverride :: Session -> String -> Maybe String -> Maybe String -> IO (Maybe Error)
networkSetUserAgentOverride session networkSetUserAgentOverrideUserAgent networkSetUserAgentOverrideAcceptLanguage networkSetUserAgentOverridePlatform = sendReceiveCommand (conn session) ("Network","setUserAgentOverride") ([("networkSetUserAgentOverrideUserAgent", ToJSONEx networkSetUserAgentOverrideUserAgent)] ++ (catMaybes [fmap (("networkSetUserAgentOverrideAcceptLanguage",) . ToJSONEx) networkSetUserAgentOverrideAcceptLanguage, fmap (("networkSetUserAgentOverridePlatform",) . ToJSONEx) networkSetUserAgentOverridePlatform]))



type PageFrameId = String

data PageFrame = PageFrame {
    pageFrameId :: PageFrameId,
    pageFrameLoaderId :: NetworkLoaderId,
    pageFrameUrl :: String,
    pageFrameSecurityOrigin :: String,
    pageFrameMimeType :: String,
    pageFrameParentId :: Maybe PageFrameId,
    pageFrameName :: Maybe String
} deriving Show
instance FromJSON  PageFrame where
    parseJSON = A.withObject "PageFrame" $ \v ->
         PageFrame <$> v .:  "id"
            <*> v  .:  "loaderId"
            <*> v  .:  "url"
            <*> v  .:  "securityOrigin"
            <*> v  .:  "mimeType"
            <*> v  .:?  "parentId"
            <*> v  .:?  "name"


instance ToJSON PageFrame  where
    toJSON v = A.object
        [ "id" .= pageFrameId v
        , "loaderId" .= pageFrameLoaderId v
        , "url" .= pageFrameUrl v
        , "securityOrigin" .= pageFrameSecurityOrigin v
        , "mimeType" .= pageFrameMimeType v
        , "parentId" .= pageFrameParentId v
        , "name" .= pageFrameName v
        ]



data PageFrameTree = PageFrameTree {
    pageFrameTreeFrame :: PageFrame,
    pageFrameTreeChildFrames :: Maybe [PageFrameTree]
} deriving Show
instance FromJSON  PageFrameTree where
    parseJSON = A.withObject "PageFrameTree" $ \v ->
         PageFrameTree <$> v .:  "frame"
            <*> v  .:?  "childFrames"


instance ToJSON PageFrameTree  where
    toJSON v = A.object
        [ "frame" .= pageFrameTreeFrame v
        , "childFrames" .= pageFrameTreeChildFrames v
        ]



type PageScriptIdentifier = String

data PageTransitionType = PageTransitionTypeLink | PageTransitionTypeTyped | PageTransitionTypeAddressBar | PageTransitionTypeAutoBookmark | PageTransitionTypeAutoSubframe | PageTransitionTypeManualSubframe | PageTransitionTypeGenerated | PageTransitionTypeAutoToplevel | PageTransitionTypeFormSubmit | PageTransitionTypeReload | PageTransitionTypeKeyword | PageTransitionTypeKeywordGenerated | PageTransitionTypeOther
    deriving Show
instance FromJSON PageTransitionType where
    parseJSON = A.withText  "PageTransitionType"  $ \v -> 
        pure $ case v of
                "link" -> PageTransitionTypeLink
                "typed" -> PageTransitionTypeTyped
                "address_bar" -> PageTransitionTypeAddressBar
                "auto_bookmark" -> PageTransitionTypeAutoBookmark
                "auto_subframe" -> PageTransitionTypeAutoSubframe
                "manual_subframe" -> PageTransitionTypeManualSubframe
                "generated" -> PageTransitionTypeGenerated
                "auto_toplevel" -> PageTransitionTypeAutoToplevel
                "form_submit" -> PageTransitionTypeFormSubmit
                "reload" -> PageTransitionTypeReload
                "keyword" -> PageTransitionTypeKeyword
                "keyword_generated" -> PageTransitionTypeKeywordGenerated
                "other" -> PageTransitionTypeOther

instance ToJSON PageTransitionType where
    toJSON v = A.String $
        case v of
                PageTransitionTypeLink -> "link"
                PageTransitionTypeTyped -> "typed"
                PageTransitionTypeAddressBar -> "address_bar"
                PageTransitionTypeAutoBookmark -> "auto_bookmark"
                PageTransitionTypeAutoSubframe -> "auto_subframe"
                PageTransitionTypeManualSubframe -> "manual_subframe"
                PageTransitionTypeGenerated -> "generated"
                PageTransitionTypeAutoToplevel -> "auto_toplevel"
                PageTransitionTypeFormSubmit -> "form_submit"
                PageTransitionTypeReload -> "reload"
                PageTransitionTypeKeyword -> "keyword"
                PageTransitionTypeKeywordGenerated -> "keyword_generated"
                PageTransitionTypeOther -> "other"



data PageNavigationEntry = PageNavigationEntry {
    pageNavigationEntryId :: Int,
    pageNavigationEntryUrl :: String,
    pageNavigationEntryUserTypedUrl :: String,
    pageNavigationEntryTitle :: String,
    pageNavigationEntryTransitionType :: PageTransitionType
} deriving Show
instance FromJSON  PageNavigationEntry where
    parseJSON = A.withObject "PageNavigationEntry" $ \v ->
         PageNavigationEntry <$> v .:  "id"
            <*> v  .:  "url"
            <*> v  .:  "userTypedURL"
            <*> v  .:  "title"
            <*> v  .:  "transitionType"


instance ToJSON PageNavigationEntry  where
    toJSON v = A.object
        [ "id" .= pageNavigationEntryId v
        , "url" .= pageNavigationEntryUrl v
        , "userTypedURL" .= pageNavigationEntryUserTypedUrl v
        , "title" .= pageNavigationEntryTitle v
        , "transitionType" .= pageNavigationEntryTransitionType v
        ]



data PageDialogType = PageDialogTypeAlert | PageDialogTypeConfirm | PageDialogTypePrompt | PageDialogTypeBeforeunload
    deriving Show
instance FromJSON PageDialogType where
    parseJSON = A.withText  "PageDialogType"  $ \v -> 
        pure $ case v of
                "alert" -> PageDialogTypeAlert
                "confirm" -> PageDialogTypeConfirm
                "prompt" -> PageDialogTypePrompt
                "beforeunload" -> PageDialogTypeBeforeunload

instance ToJSON PageDialogType where
    toJSON v = A.String $
        case v of
                PageDialogTypeAlert -> "alert"
                PageDialogTypeConfirm -> "confirm"
                PageDialogTypePrompt -> "prompt"
                PageDialogTypeBeforeunload -> "beforeunload"



data PageAppManifestError = PageAppManifestError {
    pageAppManifestErrorMessage :: String,
    pageAppManifestErrorCritical :: Int,
    pageAppManifestErrorLine :: Int,
    pageAppManifestErrorColumn :: Int
} deriving Show
instance FromJSON  PageAppManifestError where
    parseJSON = A.withObject "PageAppManifestError" $ \v ->
         PageAppManifestError <$> v .:  "message"
            <*> v  .:  "critical"
            <*> v  .:  "line"
            <*> v  .:  "column"


instance ToJSON PageAppManifestError  where
    toJSON v = A.object
        [ "message" .= pageAppManifestErrorMessage v
        , "critical" .= pageAppManifestErrorCritical v
        , "line" .= pageAppManifestErrorLine v
        , "column" .= pageAppManifestErrorColumn v
        ]



data PageLayoutViewport = PageLayoutViewport {
    pageLayoutViewportPageX :: Int,
    pageLayoutViewportPageY :: Int,
    pageLayoutViewportClientWidth :: Int,
    pageLayoutViewportClientHeight :: Int
} deriving Show
instance FromJSON  PageLayoutViewport where
    parseJSON = A.withObject "PageLayoutViewport" $ \v ->
         PageLayoutViewport <$> v .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"


instance ToJSON PageLayoutViewport  where
    toJSON v = A.object
        [ "pageX" .= pageLayoutViewportPageX v
        , "pageY" .= pageLayoutViewportPageY v
        , "clientWidth" .= pageLayoutViewportClientWidth v
        , "clientHeight" .= pageLayoutViewportClientHeight v
        ]



data PageVisualViewport = PageVisualViewport {
    pageVisualViewportOffsetX :: Int,
    pageVisualViewportOffsetY :: Int,
    pageVisualViewportPageX :: Int,
    pageVisualViewportPageY :: Int,
    pageVisualViewportClientWidth :: Int,
    pageVisualViewportClientHeight :: Int,
    pageVisualViewportScale :: Int,
    pageVisualViewportZoom :: Maybe Int
} deriving Show
instance FromJSON  PageVisualViewport where
    parseJSON = A.withObject "PageVisualViewport" $ \v ->
         PageVisualViewport <$> v .:  "offsetX"
            <*> v  .:  "offsetY"
            <*> v  .:  "pageX"
            <*> v  .:  "pageY"
            <*> v  .:  "clientWidth"
            <*> v  .:  "clientHeight"
            <*> v  .:  "scale"
            <*> v  .:?  "zoom"


instance ToJSON PageVisualViewport  where
    toJSON v = A.object
        [ "offsetX" .= pageVisualViewportOffsetX v
        , "offsetY" .= pageVisualViewportOffsetY v
        , "pageX" .= pageVisualViewportPageX v
        , "pageY" .= pageVisualViewportPageY v
        , "clientWidth" .= pageVisualViewportClientWidth v
        , "clientHeight" .= pageVisualViewportClientHeight v
        , "scale" .= pageVisualViewportScale v
        , "zoom" .= pageVisualViewportZoom v
        ]



data PageViewport = PageViewport {
    pageViewportX :: Int,
    pageViewportY :: Int,
    pageViewportWidth :: Int,
    pageViewportHeight :: Int,
    pageViewportScale :: Int
} deriving Show
instance FromJSON  PageViewport where
    parseJSON = A.withObject "PageViewport" $ \v ->
         PageViewport <$> v .:  "x"
            <*> v  .:  "y"
            <*> v  .:  "width"
            <*> v  .:  "height"
            <*> v  .:  "scale"


instance ToJSON PageViewport  where
    toJSON v = A.object
        [ "x" .= pageViewportX v
        , "y" .= pageViewportY v
        , "width" .= pageViewportWidth v
        , "height" .= pageViewportHeight v
        , "scale" .= pageViewportScale v
        ]



data PagePrerenderFinalStatus = PagePrerenderFinalStatusActivated | PagePrerenderFinalStatusDestroyed | PagePrerenderFinalStatusLowEndDevice | PagePrerenderFinalStatusCrossOriginRedirect | PagePrerenderFinalStatusCrossOriginNavigation | PagePrerenderFinalStatusInvalidSchemeRedirect | PagePrerenderFinalStatusInvalidSchemeNavigation | PagePrerenderFinalStatusInProgressNavigation | PagePrerenderFinalStatusNavigationRequestBlockedByCsp | PagePrerenderFinalStatusMainFrameNavigation | PagePrerenderFinalStatusMojoBinderPolicy | PagePrerenderFinalStatusRendererProcessCrashed | PagePrerenderFinalStatusRendererProcessKilled | PagePrerenderFinalStatusDownload | PagePrerenderFinalStatusTriggerDestroyed | PagePrerenderFinalStatusNavigationNotCommitted | PagePrerenderFinalStatusNavigationBadHttpStatus | PagePrerenderFinalStatusClientCertRequested | PagePrerenderFinalStatusNavigationRequestNetworkError | PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded | PagePrerenderFinalStatusCancelAllHostsForTesting | PagePrerenderFinalStatusDidFailLoad | PagePrerenderFinalStatusStop | PagePrerenderFinalStatusSslCertificateError | PagePrerenderFinalStatusLoginAuthRequested | PagePrerenderFinalStatusUaChangeRequiresReload | PagePrerenderFinalStatusBlockedByClient | PagePrerenderFinalStatusAudioOutputDeviceRequested | PagePrerenderFinalStatusMixedContent | PagePrerenderFinalStatusTriggerBackgrounded | PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected | PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed
    deriving Show
instance FromJSON PagePrerenderFinalStatus where
    parseJSON = A.withText  "PagePrerenderFinalStatus"  $ \v -> 
        pure $ case v of
                "Activated" -> PagePrerenderFinalStatusActivated
                "Destroyed" -> PagePrerenderFinalStatusDestroyed
                "LowEndDevice" -> PagePrerenderFinalStatusLowEndDevice
                "CrossOriginRedirect" -> PagePrerenderFinalStatusCrossOriginRedirect
                "CrossOriginNavigation" -> PagePrerenderFinalStatusCrossOriginNavigation
                "InvalidSchemeRedirect" -> PagePrerenderFinalStatusInvalidSchemeRedirect
                "InvalidSchemeNavigation" -> PagePrerenderFinalStatusInvalidSchemeNavigation
                "InProgressNavigation" -> PagePrerenderFinalStatusInProgressNavigation
                "NavigationRequestBlockedByCsp" -> PagePrerenderFinalStatusNavigationRequestBlockedByCsp
                "MainFrameNavigation" -> PagePrerenderFinalStatusMainFrameNavigation
                "MojoBinderPolicy" -> PagePrerenderFinalStatusMojoBinderPolicy
                "RendererProcessCrashed" -> PagePrerenderFinalStatusRendererProcessCrashed
                "RendererProcessKilled" -> PagePrerenderFinalStatusRendererProcessKilled
                "Download" -> PagePrerenderFinalStatusDownload
                "TriggerDestroyed" -> PagePrerenderFinalStatusTriggerDestroyed
                "NavigationNotCommitted" -> PagePrerenderFinalStatusNavigationNotCommitted
                "NavigationBadHttpStatus" -> PagePrerenderFinalStatusNavigationBadHttpStatus
                "ClientCertRequested" -> PagePrerenderFinalStatusClientCertRequested
                "NavigationRequestNetworkError" -> PagePrerenderFinalStatusNavigationRequestNetworkError
                "MaxNumOfRunningPrerendersExceeded" -> PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded
                "CancelAllHostsForTesting" -> PagePrerenderFinalStatusCancelAllHostsForTesting
                "DidFailLoad" -> PagePrerenderFinalStatusDidFailLoad
                "Stop" -> PagePrerenderFinalStatusStop
                "SslCertificateError" -> PagePrerenderFinalStatusSslCertificateError
                "LoginAuthRequested" -> PagePrerenderFinalStatusLoginAuthRequested
                "UaChangeRequiresReload" -> PagePrerenderFinalStatusUaChangeRequiresReload
                "BlockedByClient" -> PagePrerenderFinalStatusBlockedByClient
                "AudioOutputDeviceRequested" -> PagePrerenderFinalStatusAudioOutputDeviceRequested
                "MixedContent" -> PagePrerenderFinalStatusMixedContent
                "TriggerBackgrounded" -> PagePrerenderFinalStatusTriggerBackgrounded
                "EmbedderTriggeredAndSameOriginRedirected" -> PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected
                "EmbedderTriggeredAndCrossOriginRedirected" -> PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected
                "EmbedderTriggeredAndDestroyed" -> PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed

instance ToJSON PagePrerenderFinalStatus where
    toJSON v = A.String $
        case v of
                PagePrerenderFinalStatusActivated -> "Activated"
                PagePrerenderFinalStatusDestroyed -> "Destroyed"
                PagePrerenderFinalStatusLowEndDevice -> "LowEndDevice"
                PagePrerenderFinalStatusCrossOriginRedirect -> "CrossOriginRedirect"
                PagePrerenderFinalStatusCrossOriginNavigation -> "CrossOriginNavigation"
                PagePrerenderFinalStatusInvalidSchemeRedirect -> "InvalidSchemeRedirect"
                PagePrerenderFinalStatusInvalidSchemeNavigation -> "InvalidSchemeNavigation"
                PagePrerenderFinalStatusInProgressNavigation -> "InProgressNavigation"
                PagePrerenderFinalStatusNavigationRequestBlockedByCsp -> "NavigationRequestBlockedByCsp"
                PagePrerenderFinalStatusMainFrameNavigation -> "MainFrameNavigation"
                PagePrerenderFinalStatusMojoBinderPolicy -> "MojoBinderPolicy"
                PagePrerenderFinalStatusRendererProcessCrashed -> "RendererProcessCrashed"
                PagePrerenderFinalStatusRendererProcessKilled -> "RendererProcessKilled"
                PagePrerenderFinalStatusDownload -> "Download"
                PagePrerenderFinalStatusTriggerDestroyed -> "TriggerDestroyed"
                PagePrerenderFinalStatusNavigationNotCommitted -> "NavigationNotCommitted"
                PagePrerenderFinalStatusNavigationBadHttpStatus -> "NavigationBadHttpStatus"
                PagePrerenderFinalStatusClientCertRequested -> "ClientCertRequested"
                PagePrerenderFinalStatusNavigationRequestNetworkError -> "NavigationRequestNetworkError"
                PagePrerenderFinalStatusMaxNumOfRunningPrerendersExceeded -> "MaxNumOfRunningPrerendersExceeded"
                PagePrerenderFinalStatusCancelAllHostsForTesting -> "CancelAllHostsForTesting"
                PagePrerenderFinalStatusDidFailLoad -> "DidFailLoad"
                PagePrerenderFinalStatusStop -> "Stop"
                PagePrerenderFinalStatusSslCertificateError -> "SslCertificateError"
                PagePrerenderFinalStatusLoginAuthRequested -> "LoginAuthRequested"
                PagePrerenderFinalStatusUaChangeRequiresReload -> "UaChangeRequiresReload"
                PagePrerenderFinalStatusBlockedByClient -> "BlockedByClient"
                PagePrerenderFinalStatusAudioOutputDeviceRequested -> "AudioOutputDeviceRequested"
                PagePrerenderFinalStatusMixedContent -> "MixedContent"
                PagePrerenderFinalStatusTriggerBackgrounded -> "TriggerBackgrounded"
                PagePrerenderFinalStatusEmbedderTriggeredAndSameOriginRedirected -> "EmbedderTriggeredAndSameOriginRedirected"
                PagePrerenderFinalStatusEmbedderTriggeredAndCrossOriginRedirected -> "EmbedderTriggeredAndCrossOriginRedirected"
                PagePrerenderFinalStatusEmbedderTriggeredAndDestroyed -> "EmbedderTriggeredAndDestroyed"


data PageAddScriptToEvaluateOnNewDocument = PageAddScriptToEvaluateOnNewDocument {
    pageAddScriptToEvaluateOnNewDocumentIdentifier :: PageScriptIdentifier
} deriving Show
instance FromJSON  PageAddScriptToEvaluateOnNewDocument where
    parseJSON = A.withObject "PageAddScriptToEvaluateOnNewDocument" $ \v ->
         PageAddScriptToEvaluateOnNewDocument <$> v .:  "identifier"



pageAddScriptToEvaluateOnNewDocument :: Session -> String -> IO (Either Error PageAddScriptToEvaluateOnNewDocument)
pageAddScriptToEvaluateOnNewDocument session pageAddScriptToEvaluateOnNewDocumentSource = sendReceiveCommandResult (conn session) ("Page","addScriptToEvaluateOnNewDocument") ([("pageAddScriptToEvaluateOnNewDocumentSource", ToJSONEx pageAddScriptToEvaluateOnNewDocumentSource)] ++ (catMaybes []))


pageBringToFront :: Session -> IO (Maybe Error)
pageBringToFront session  = sendReceiveCommand (conn session) ("Page","bringToFront") ([] ++ (catMaybes []))

data PageCaptureScreenshot = PageCaptureScreenshot {
    pageCaptureScreenshotData :: String
} deriving Show
instance FromJSON  PageCaptureScreenshot where
    parseJSON = A.withObject "PageCaptureScreenshot" $ \v ->
         PageCaptureScreenshot <$> v .:  "data"



pageCaptureScreenshot :: Session -> Maybe String -> Maybe Int -> Maybe PageViewport -> IO (Either Error PageCaptureScreenshot)
pageCaptureScreenshot session pageCaptureScreenshotFormat pageCaptureScreenshotQuality pageCaptureScreenshotClip = sendReceiveCommandResult (conn session) ("Page","captureScreenshot") ([] ++ (catMaybes [fmap (("pageCaptureScreenshotFormat",) . ToJSONEx) pageCaptureScreenshotFormat, fmap (("pageCaptureScreenshotQuality",) . ToJSONEx) pageCaptureScreenshotQuality, fmap (("pageCaptureScreenshotClip",) . ToJSONEx) pageCaptureScreenshotClip]))

data PageCreateIsolatedWorld = PageCreateIsolatedWorld {
    pageCreateIsolatedWorldExecutionContextId :: RuntimeExecutionContextId
} deriving Show
instance FromJSON  PageCreateIsolatedWorld where
    parseJSON = A.withObject "PageCreateIsolatedWorld" $ \v ->
         PageCreateIsolatedWorld <$> v .:  "executionContextId"



pageCreateIsolatedWorld :: Session -> PageFrameId -> Maybe String -> Maybe Bool -> IO (Either Error PageCreateIsolatedWorld)
pageCreateIsolatedWorld session pageCreateIsolatedWorldFrameId pageCreateIsolatedWorldWorldName pageCreateIsolatedWorldGrantUniveralAccess = sendReceiveCommandResult (conn session) ("Page","createIsolatedWorld") ([("pageCreateIsolatedWorldFrameId", ToJSONEx pageCreateIsolatedWorldFrameId)] ++ (catMaybes [fmap (("pageCreateIsolatedWorldWorldName",) . ToJSONEx) pageCreateIsolatedWorldWorldName, fmap (("pageCreateIsolatedWorldGrantUniveralAccess",) . ToJSONEx) pageCreateIsolatedWorldGrantUniveralAccess]))


pageDisable :: Session -> IO (Maybe Error)
pageDisable session  = sendReceiveCommand (conn session) ("Page","disable") ([] ++ (catMaybes []))


pageEnable :: Session -> IO (Maybe Error)
pageEnable session  = sendReceiveCommand (conn session) ("Page","enable") ([] ++ (catMaybes []))

data PageGetAppManifest = PageGetAppManifest {
    pageGetAppManifestUrl :: String,
    pageGetAppManifestErrors :: [PageAppManifestError],
    pageGetAppManifestData :: Maybe String
} deriving Show
instance FromJSON  PageGetAppManifest where
    parseJSON = A.withObject "PageGetAppManifest" $ \v ->
         PageGetAppManifest <$> v .:  "url"
            <*> v  .:  "errors"
            <*> v  .:?  "data"



pageGetAppManifest :: Session -> IO (Either Error PageGetAppManifest)
pageGetAppManifest session  = sendReceiveCommandResult (conn session) ("Page","getAppManifest") ([] ++ (catMaybes []))

data PageGetFrameTree = PageGetFrameTree {
    pageGetFrameTreeFrameTree :: PageFrameTree
} deriving Show
instance FromJSON  PageGetFrameTree where
    parseJSON = A.withObject "PageGetFrameTree" $ \v ->
         PageGetFrameTree <$> v .:  "frameTree"



pageGetFrameTree :: Session -> IO (Either Error PageGetFrameTree)
pageGetFrameTree session  = sendReceiveCommandResult (conn session) ("Page","getFrameTree") ([] ++ (catMaybes []))

data PageGetLayoutMetrics = PageGetLayoutMetrics {
    pageGetLayoutMetricsCssLayoutViewport :: PageLayoutViewport,
    pageGetLayoutMetricsCssVisualViewport :: PageVisualViewport,
    pageGetLayoutMetricsCssContentSize :: DOMRect
} deriving Show
instance FromJSON  PageGetLayoutMetrics where
    parseJSON = A.withObject "PageGetLayoutMetrics" $ \v ->
         PageGetLayoutMetrics <$> v .:  "cssLayoutViewport"
            <*> v  .:  "cssVisualViewport"
            <*> v  .:  "cssContentSize"



pageGetLayoutMetrics :: Session -> IO (Either Error PageGetLayoutMetrics)
pageGetLayoutMetrics session  = sendReceiveCommandResult (conn session) ("Page","getLayoutMetrics") ([] ++ (catMaybes []))

data PageGetNavigationHistory = PageGetNavigationHistory {
    pageGetNavigationHistoryCurrentIndex :: Int,
    pageGetNavigationHistoryEntries :: [PageNavigationEntry]
} deriving Show
instance FromJSON  PageGetNavigationHistory where
    parseJSON = A.withObject "PageGetNavigationHistory" $ \v ->
         PageGetNavigationHistory <$> v .:  "currentIndex"
            <*> v  .:  "entries"



pageGetNavigationHistory :: Session -> IO (Either Error PageGetNavigationHistory)
pageGetNavigationHistory session  = sendReceiveCommandResult (conn session) ("Page","getNavigationHistory") ([] ++ (catMaybes []))


pageResetNavigationHistory :: Session -> IO (Maybe Error)
pageResetNavigationHistory session  = sendReceiveCommand (conn session) ("Page","resetNavigationHistory") ([] ++ (catMaybes []))


pageHandleJavaScriptDialog :: Session -> Bool -> Maybe String -> IO (Maybe Error)
pageHandleJavaScriptDialog session pageHandleJavaScriptDialogAccept pageHandleJavaScriptDialogPromptText = sendReceiveCommand (conn session) ("Page","handleJavaScriptDialog") ([("pageHandleJavaScriptDialogAccept", ToJSONEx pageHandleJavaScriptDialogAccept)] ++ (catMaybes [fmap (("pageHandleJavaScriptDialogPromptText",) . ToJSONEx) pageHandleJavaScriptDialogPromptText]))

data PageNavigate = PageNavigate {
    pageNavigateFrameId :: PageFrameId,
    pageNavigateLoaderId :: Maybe NetworkLoaderId,
    pageNavigateErrorText :: Maybe String
} deriving Show
instance FromJSON  PageNavigate where
    parseJSON = A.withObject "PageNavigate" $ \v ->
         PageNavigate <$> v .:  "frameId"
            <*> v  .:?  "loaderId"
            <*> v  .:?  "errorText"



pageNavigate :: Session -> String -> Maybe String -> Maybe PageTransitionType -> Maybe PageFrameId -> IO (Either Error PageNavigate)
pageNavigate session pageNavigateUrl pageNavigateReferrer pageNavigateTransitionType pageNavigateFrameId = sendReceiveCommandResult (conn session) ("Page","navigate") ([("pageNavigateUrl", ToJSONEx pageNavigateUrl)] ++ (catMaybes [fmap (("pageNavigateReferrer",) . ToJSONEx) pageNavigateReferrer, fmap (("pageNavigateTransitionType",) . ToJSONEx) pageNavigateTransitionType, fmap (("pageNavigateFrameId",) . ToJSONEx) pageNavigateFrameId]))


pageNavigateToHistoryEntry :: Session -> Int -> IO (Maybe Error)
pageNavigateToHistoryEntry session pageNavigateToHistoryEntryEntryId = sendReceiveCommand (conn session) ("Page","navigateToHistoryEntry") ([("pageNavigateToHistoryEntryEntryId", ToJSONEx pageNavigateToHistoryEntryEntryId)] ++ (catMaybes []))

data PagePrintToPDF = PagePrintToPDF {
    pagePrintToPDFData :: String
} deriving Show
instance FromJSON  PagePrintToPDF where
    parseJSON = A.withObject "PagePrintToPDF" $ \v ->
         PagePrintToPDF <$> v .:  "data"



pagePrintToPDF :: Session -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> IO (Either Error PagePrintToPDF)
pagePrintToPDF session pagePrintToPDFLandscape pagePrintToPDFDisplayHeaderFooter pagePrintToPDFPrintBackground pagePrintToPDFScale pagePrintToPDFPaperWidth pagePrintToPDFPaperHeight pagePrintToPDFMarginTop pagePrintToPDFMarginBottom pagePrintToPDFMarginLeft pagePrintToPDFMarginRight pagePrintToPDFPageRanges pagePrintToPDFHeaderTemplate pagePrintToPDFFooterTemplate pagePrintToPDFPreferCssPageSize = sendReceiveCommandResult (conn session) ("Page","printToPDF") ([] ++ (catMaybes [fmap (("pagePrintToPDFLandscape",) . ToJSONEx) pagePrintToPDFLandscape, fmap (("pagePrintToPDFDisplayHeaderFooter",) . ToJSONEx) pagePrintToPDFDisplayHeaderFooter, fmap (("pagePrintToPDFPrintBackground",) . ToJSONEx) pagePrintToPDFPrintBackground, fmap (("pagePrintToPDFScale",) . ToJSONEx) pagePrintToPDFScale, fmap (("pagePrintToPDFPaperWidth",) . ToJSONEx) pagePrintToPDFPaperWidth, fmap (("pagePrintToPDFPaperHeight",) . ToJSONEx) pagePrintToPDFPaperHeight, fmap (("pagePrintToPDFMarginTop",) . ToJSONEx) pagePrintToPDFMarginTop, fmap (("pagePrintToPDFMarginBottom",) . ToJSONEx) pagePrintToPDFMarginBottom, fmap (("pagePrintToPDFMarginLeft",) . ToJSONEx) pagePrintToPDFMarginLeft, fmap (("pagePrintToPDFMarginRight",) . ToJSONEx) pagePrintToPDFMarginRight, fmap (("pagePrintToPDFPageRanges",) . ToJSONEx) pagePrintToPDFPageRanges, fmap (("pagePrintToPDFHeaderTemplate",) . ToJSONEx) pagePrintToPDFHeaderTemplate, fmap (("pagePrintToPDFFooterTemplate",) . ToJSONEx) pagePrintToPDFFooterTemplate, fmap (("pagePrintToPDFPreferCssPageSize",) . ToJSONEx) pagePrintToPDFPreferCssPageSize]))


pageReload :: Session -> Maybe Bool -> Maybe String -> IO (Maybe Error)
pageReload session pageReloadIgnoreCache pageReloadScriptToEvaluateOnLoad = sendReceiveCommand (conn session) ("Page","reload") ([] ++ (catMaybes [fmap (("pageReloadIgnoreCache",) . ToJSONEx) pageReloadIgnoreCache, fmap (("pageReloadScriptToEvaluateOnLoad",) . ToJSONEx) pageReloadScriptToEvaluateOnLoad]))


pageRemoveScriptToEvaluateOnNewDocument :: Session -> PageScriptIdentifier -> IO (Maybe Error)
pageRemoveScriptToEvaluateOnNewDocument session pageRemoveScriptToEvaluateOnNewDocumentIdentifier = sendReceiveCommand (conn session) ("Page","removeScriptToEvaluateOnNewDocument") ([("pageRemoveScriptToEvaluateOnNewDocumentIdentifier", ToJSONEx pageRemoveScriptToEvaluateOnNewDocumentIdentifier)] ++ (catMaybes []))


pageSetDocumentContent :: Session -> PageFrameId -> String -> IO (Maybe Error)
pageSetDocumentContent session pageSetDocumentContentFrameId pageSetDocumentContentHtml = sendReceiveCommand (conn session) ("Page","setDocumentContent") ([("pageSetDocumentContentFrameId", ToJSONEx pageSetDocumentContentFrameId), ("pageSetDocumentContentHtml", ToJSONEx pageSetDocumentContentHtml)] ++ (catMaybes []))


pageStopLoading :: Session -> IO (Maybe Error)
pageStopLoading session  = sendReceiveCommand (conn session) ("Page","stopLoading") ([] ++ (catMaybes []))



data PerformanceMetric = PerformanceMetric {
    performanceMetricName :: String,
    performanceMetricValue :: Int
} deriving Show
instance FromJSON  PerformanceMetric where
    parseJSON = A.withObject "PerformanceMetric" $ \v ->
         PerformanceMetric <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON PerformanceMetric  where
    toJSON v = A.object
        [ "name" .= performanceMetricName v
        , "value" .= performanceMetricValue v
        ]



performanceDisable :: Session -> IO (Maybe Error)
performanceDisable session  = sendReceiveCommand (conn session) ("Performance","disable") ([] ++ (catMaybes []))


performanceEnable :: Session -> Maybe String -> IO (Maybe Error)
performanceEnable session performanceEnableTimeDomain = sendReceiveCommand (conn session) ("Performance","enable") ([] ++ (catMaybes [fmap (("performanceEnableTimeDomain",) . ToJSONEx) performanceEnableTimeDomain]))

data PerformanceGetMetrics = PerformanceGetMetrics {
    performanceGetMetricsMetrics :: [PerformanceMetric]
} deriving Show
instance FromJSON  PerformanceGetMetrics where
    parseJSON = A.withObject "PerformanceGetMetrics" $ \v ->
         PerformanceGetMetrics <$> v .:  "metrics"



performanceGetMetrics :: Session -> IO (Either Error PerformanceGetMetrics)
performanceGetMetrics session  = sendReceiveCommandResult (conn session) ("Performance","getMetrics") ([] ++ (catMaybes []))



type SecurityCertificateId = Int

data SecurityMixedContentType = SecurityMixedContentTypeBlockable | SecurityMixedContentTypeOptionallyBlockable | SecurityMixedContentTypeNone
    deriving Show
instance FromJSON SecurityMixedContentType where
    parseJSON = A.withText  "SecurityMixedContentType"  $ \v -> 
        pure $ case v of
                "blockable" -> SecurityMixedContentTypeBlockable
                "optionally-blockable" -> SecurityMixedContentTypeOptionallyBlockable
                "none" -> SecurityMixedContentTypeNone

instance ToJSON SecurityMixedContentType where
    toJSON v = A.String $
        case v of
                SecurityMixedContentTypeBlockable -> "blockable"
                SecurityMixedContentTypeOptionallyBlockable -> "optionally-blockable"
                SecurityMixedContentTypeNone -> "none"



data SecuritySecurityState = SecuritySecurityStateUnknown | SecuritySecurityStateNeutral | SecuritySecurityStateInsecure | SecuritySecurityStateSecure | SecuritySecurityStateInfo | SecuritySecurityStateInsecureBroken
    deriving Show
instance FromJSON SecuritySecurityState where
    parseJSON = A.withText  "SecuritySecurityState"  $ \v -> 
        pure $ case v of
                "unknown" -> SecuritySecurityStateUnknown
                "neutral" -> SecuritySecurityStateNeutral
                "insecure" -> SecuritySecurityStateInsecure
                "secure" -> SecuritySecurityStateSecure
                "info" -> SecuritySecurityStateInfo
                "insecure-broken" -> SecuritySecurityStateInsecureBroken

instance ToJSON SecuritySecurityState where
    toJSON v = A.String $
        case v of
                SecuritySecurityStateUnknown -> "unknown"
                SecuritySecurityStateNeutral -> "neutral"
                SecuritySecurityStateInsecure -> "insecure"
                SecuritySecurityStateSecure -> "secure"
                SecuritySecurityStateInfo -> "info"
                SecuritySecurityStateInsecureBroken -> "insecure-broken"



data SecuritySecurityStateExplanation = SecuritySecurityStateExplanation {
    securitySecurityStateExplanationSecurityState :: SecuritySecurityState,
    securitySecurityStateExplanationTitle :: String,
    securitySecurityStateExplanationSummary :: String,
    securitySecurityStateExplanationDescription :: String,
    securitySecurityStateExplanationMixedContentType :: SecurityMixedContentType,
    securitySecurityStateExplanationCertificate :: [String],
    securitySecurityStateExplanationRecommendations :: Maybe [String]
} deriving Show
instance FromJSON  SecuritySecurityStateExplanation where
    parseJSON = A.withObject "SecuritySecurityStateExplanation" $ \v ->
         SecuritySecurityStateExplanation <$> v .:  "securityState"
            <*> v  .:  "title"
            <*> v  .:  "summary"
            <*> v  .:  "description"
            <*> v  .:  "mixedContentType"
            <*> v  .:  "certificate"
            <*> v  .:?  "recommendations"


instance ToJSON SecuritySecurityStateExplanation  where
    toJSON v = A.object
        [ "securityState" .= securitySecurityStateExplanationSecurityState v
        , "title" .= securitySecurityStateExplanationTitle v
        , "summary" .= securitySecurityStateExplanationSummary v
        , "description" .= securitySecurityStateExplanationDescription v
        , "mixedContentType" .= securitySecurityStateExplanationMixedContentType v
        , "certificate" .= securitySecurityStateExplanationCertificate v
        , "recommendations" .= securitySecurityStateExplanationRecommendations v
        ]



data SecurityCertificateErrorAction = SecurityCertificateErrorActionContinue | SecurityCertificateErrorActionCancel
    deriving Show
instance FromJSON SecurityCertificateErrorAction where
    parseJSON = A.withText  "SecurityCertificateErrorAction"  $ \v -> 
        pure $ case v of
                "continue" -> SecurityCertificateErrorActionContinue
                "cancel" -> SecurityCertificateErrorActionCancel

instance ToJSON SecurityCertificateErrorAction where
    toJSON v = A.String $
        case v of
                SecurityCertificateErrorActionContinue -> "continue"
                SecurityCertificateErrorActionCancel -> "cancel"



securityDisable :: Session -> IO (Maybe Error)
securityDisable session  = sendReceiveCommand (conn session) ("Security","disable") ([] ++ (catMaybes []))


securityEnable :: Session -> IO (Maybe Error)
securityEnable session  = sendReceiveCommand (conn session) ("Security","enable") ([] ++ (catMaybes []))



type TargetTargetID = String

type TargetSessionID = String

data TargetTargetInfo = TargetTargetInfo {
    targetTargetInfoTargetId :: TargetTargetID,
    targetTargetInfoType :: String,
    targetTargetInfoTitle :: String,
    targetTargetInfoUrl :: String,
    targetTargetInfoAttached :: Bool,
    targetTargetInfoOpenerId :: Maybe TargetTargetID
} deriving Show
instance FromJSON  TargetTargetInfo where
    parseJSON = A.withObject "TargetTargetInfo" $ \v ->
         TargetTargetInfo <$> v .:  "targetId"
            <*> v  .:  "type"
            <*> v  .:  "title"
            <*> v  .:  "url"
            <*> v  .:  "attached"
            <*> v  .:?  "openerId"


instance ToJSON TargetTargetInfo  where
    toJSON v = A.object
        [ "targetId" .= targetTargetInfoTargetId v
        , "type" .= targetTargetInfoType v
        , "title" .= targetTargetInfoTitle v
        , "url" .= targetTargetInfoUrl v
        , "attached" .= targetTargetInfoAttached v
        , "openerId" .= targetTargetInfoOpenerId v
        ]



targetActivateTarget :: Session -> TargetTargetID -> IO (Maybe Error)
targetActivateTarget session targetActivateTargetTargetId = sendReceiveCommand (conn session) ("Target","activateTarget") ([("targetActivateTargetTargetId", ToJSONEx targetActivateTargetTargetId)] ++ (catMaybes []))

data TargetAttachToTarget = TargetAttachToTarget {
    targetAttachToTargetSessionId :: TargetSessionID
} deriving Show
instance FromJSON  TargetAttachToTarget where
    parseJSON = A.withObject "TargetAttachToTarget" $ \v ->
         TargetAttachToTarget <$> v .:  "sessionId"



targetAttachToTarget :: Session -> TargetTargetID -> Maybe Bool -> IO (Either Error TargetAttachToTarget)
targetAttachToTarget session targetAttachToTargetTargetId targetAttachToTargetFlatten = sendReceiveCommandResult (conn session) ("Target","attachToTarget") ([("targetAttachToTargetTargetId", ToJSONEx targetAttachToTargetTargetId)] ++ (catMaybes [fmap (("targetAttachToTargetFlatten",) . ToJSONEx) targetAttachToTargetFlatten]))


targetCloseTarget :: Session -> TargetTargetID -> IO (Maybe Error)
targetCloseTarget session targetCloseTargetTargetId = sendReceiveCommand (conn session) ("Target","closeTarget") ([("targetCloseTargetTargetId", ToJSONEx targetCloseTargetTargetId)] ++ (catMaybes []))

data TargetCreateTarget = TargetCreateTarget {
    targetCreateTargetTargetId :: TargetTargetID
} deriving Show
instance FromJSON  TargetCreateTarget where
    parseJSON = A.withObject "TargetCreateTarget" $ \v ->
         TargetCreateTarget <$> v .:  "targetId"



targetCreateTarget :: Session -> String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> IO (Either Error TargetCreateTarget)
targetCreateTarget session targetCreateTargetUrl targetCreateTargetWidth targetCreateTargetHeight targetCreateTargetNewWindow targetCreateTargetBackground = sendReceiveCommandResult (conn session) ("Target","createTarget") ([("targetCreateTargetUrl", ToJSONEx targetCreateTargetUrl)] ++ (catMaybes [fmap (("targetCreateTargetWidth",) . ToJSONEx) targetCreateTargetWidth, fmap (("targetCreateTargetHeight",) . ToJSONEx) targetCreateTargetHeight, fmap (("targetCreateTargetNewWindow",) . ToJSONEx) targetCreateTargetNewWindow, fmap (("targetCreateTargetBackground",) . ToJSONEx) targetCreateTargetBackground]))


targetDetachFromTarget :: Session -> Maybe TargetSessionID -> IO (Maybe Error)
targetDetachFromTarget session targetDetachFromTargetSessionId = sendReceiveCommand (conn session) ("Target","detachFromTarget") ([] ++ (catMaybes [fmap (("targetDetachFromTargetSessionId",) . ToJSONEx) targetDetachFromTargetSessionId]))

data TargetGetTargets = TargetGetTargets {
    targetGetTargetsTargetInfos :: [TargetTargetInfo]
} deriving Show
instance FromJSON  TargetGetTargets where
    parseJSON = A.withObject "TargetGetTargets" $ \v ->
         TargetGetTargets <$> v .:  "targetInfos"



targetGetTargets :: Session -> IO (Either Error TargetGetTargets)
targetGetTargets session  = sendReceiveCommandResult (conn session) ("Target","getTargets") ([] ++ (catMaybes []))


targetSetDiscoverTargets :: Session -> Bool -> IO (Maybe Error)
targetSetDiscoverTargets session targetSetDiscoverTargetsDiscover = sendReceiveCommand (conn session) ("Target","setDiscoverTargets") ([("targetSetDiscoverTargetsDiscover", ToJSONEx targetSetDiscoverTargetsDiscover)] ++ (catMaybes []))



type FetchRequestId = String

data FetchRequestStage = FetchRequestStageRequest | FetchRequestStageResponse
    deriving Show
instance FromJSON FetchRequestStage where
    parseJSON = A.withText  "FetchRequestStage"  $ \v -> 
        pure $ case v of
                "Request" -> FetchRequestStageRequest
                "Response" -> FetchRequestStageResponse

instance ToJSON FetchRequestStage where
    toJSON v = A.String $
        case v of
                FetchRequestStageRequest -> "Request"
                FetchRequestStageResponse -> "Response"



data FetchRequestPattern = FetchRequestPattern {
    fetchRequestPatternUrlPattern :: Maybe String,
    fetchRequestPatternResourceType :: Maybe NetworkResourceType,
    fetchRequestPatternRequestStage :: Maybe FetchRequestStage
} deriving Show
instance FromJSON  FetchRequestPattern where
    parseJSON = A.withObject "FetchRequestPattern" $ \v ->
         FetchRequestPattern <$> v .:?  "urlPattern"
            <*> v  .:?  "resourceType"
            <*> v  .:?  "requestStage"


instance ToJSON FetchRequestPattern  where
    toJSON v = A.object
        [ "urlPattern" .= fetchRequestPatternUrlPattern v
        , "resourceType" .= fetchRequestPatternResourceType v
        , "requestStage" .= fetchRequestPatternRequestStage v
        ]



data FetchHeaderEntry = FetchHeaderEntry {
    fetchHeaderEntryName :: String,
    fetchHeaderEntryValue :: String
} deriving Show
instance FromJSON  FetchHeaderEntry where
    parseJSON = A.withObject "FetchHeaderEntry" $ \v ->
         FetchHeaderEntry <$> v .:  "name"
            <*> v  .:  "value"


instance ToJSON FetchHeaderEntry  where
    toJSON v = A.object
        [ "name" .= fetchHeaderEntryName v
        , "value" .= fetchHeaderEntryValue v
        ]



data FetchAuthChallenge = FetchAuthChallenge {
    fetchAuthChallengeOrigin :: String,
    fetchAuthChallengeScheme :: String,
    fetchAuthChallengeRealm :: String,
    fetchAuthChallengeSource :: Maybe String
} deriving Show
instance FromJSON  FetchAuthChallenge where
    parseJSON = A.withObject "FetchAuthChallenge" $ \v ->
         FetchAuthChallenge <$> v .:  "origin"
            <*> v  .:  "scheme"
            <*> v  .:  "realm"
            <*> v  .:?  "source"


instance ToJSON FetchAuthChallenge  where
    toJSON v = A.object
        [ "origin" .= fetchAuthChallengeOrigin v
        , "scheme" .= fetchAuthChallengeScheme v
        , "realm" .= fetchAuthChallengeRealm v
        , "source" .= fetchAuthChallengeSource v
        ]



data FetchAuthChallengeResponse = FetchAuthChallengeResponse {
    fetchAuthChallengeResponseResponse :: String,
    fetchAuthChallengeResponseUsername :: Maybe String,
    fetchAuthChallengeResponsePassword :: Maybe String
} deriving Show
instance FromJSON  FetchAuthChallengeResponse where
    parseJSON = A.withObject "FetchAuthChallengeResponse" $ \v ->
         FetchAuthChallengeResponse <$> v .:  "response"
            <*> v  .:?  "username"
            <*> v  .:?  "password"


instance ToJSON FetchAuthChallengeResponse  where
    toJSON v = A.object
        [ "response" .= fetchAuthChallengeResponseResponse v
        , "username" .= fetchAuthChallengeResponseUsername v
        , "password" .= fetchAuthChallengeResponsePassword v
        ]



fetchDisable :: Session -> IO (Maybe Error)
fetchDisable session  = sendReceiveCommand (conn session) ("Fetch","disable") ([] ++ (catMaybes []))


fetchEnable :: Session -> Maybe [FetchRequestPattern] -> Maybe Bool -> IO (Maybe Error)
fetchEnable session fetchEnablePatterns fetchEnableHandleAuthRequests = sendReceiveCommand (conn session) ("Fetch","enable") ([] ++ (catMaybes [fmap (("fetchEnablePatterns",) . ToJSONEx) fetchEnablePatterns, fmap (("fetchEnableHandleAuthRequests",) . ToJSONEx) fetchEnableHandleAuthRequests]))


fetchFailRequest :: Session -> FetchRequestId -> NetworkErrorReason -> IO (Maybe Error)
fetchFailRequest session fetchFailRequestRequestId fetchFailRequestErrorReason = sendReceiveCommand (conn session) ("Fetch","failRequest") ([("fetchFailRequestRequestId", ToJSONEx fetchFailRequestRequestId), ("fetchFailRequestErrorReason", ToJSONEx fetchFailRequestErrorReason)] ++ (catMaybes []))


fetchFulfillRequest :: Session -> FetchRequestId -> Int -> Maybe [FetchHeaderEntry] -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Error)
fetchFulfillRequest session fetchFulfillRequestRequestId fetchFulfillRequestResponseCode fetchFulfillRequestResponseHeaders fetchFulfillRequestBinaryResponseHeaders fetchFulfillRequestBody fetchFulfillRequestResponsePhrase = sendReceiveCommand (conn session) ("Fetch","fulfillRequest") ([("fetchFulfillRequestRequestId", ToJSONEx fetchFulfillRequestRequestId), ("fetchFulfillRequestResponseCode", ToJSONEx fetchFulfillRequestResponseCode)] ++ (catMaybes [fmap (("fetchFulfillRequestResponseHeaders",) . ToJSONEx) fetchFulfillRequestResponseHeaders, fmap (("fetchFulfillRequestBinaryResponseHeaders",) . ToJSONEx) fetchFulfillRequestBinaryResponseHeaders, fmap (("fetchFulfillRequestBody",) . ToJSONEx) fetchFulfillRequestBody, fmap (("fetchFulfillRequestResponsePhrase",) . ToJSONEx) fetchFulfillRequestResponsePhrase]))


fetchContinueRequest :: Session -> FetchRequestId -> Maybe String -> Maybe String -> Maybe String -> Maybe [FetchHeaderEntry] -> IO (Maybe Error)
fetchContinueRequest session fetchContinueRequestRequestId fetchContinueRequestUrl fetchContinueRequestMethod fetchContinueRequestPostData fetchContinueRequestHeaders = sendReceiveCommand (conn session) ("Fetch","continueRequest") ([("fetchContinueRequestRequestId", ToJSONEx fetchContinueRequestRequestId)] ++ (catMaybes [fmap (("fetchContinueRequestUrl",) . ToJSONEx) fetchContinueRequestUrl, fmap (("fetchContinueRequestMethod",) . ToJSONEx) fetchContinueRequestMethod, fmap (("fetchContinueRequestPostData",) . ToJSONEx) fetchContinueRequestPostData, fmap (("fetchContinueRequestHeaders",) . ToJSONEx) fetchContinueRequestHeaders]))


fetchContinueWithAuth :: Session -> FetchRequestId -> FetchAuthChallengeResponse -> IO (Maybe Error)
fetchContinueWithAuth session fetchContinueWithAuthRequestId fetchContinueWithAuthAuthChallengeResponse = sendReceiveCommand (conn session) ("Fetch","continueWithAuth") ([("fetchContinueWithAuthRequestId", ToJSONEx fetchContinueWithAuthRequestId), ("fetchContinueWithAuthAuthChallengeResponse", ToJSONEx fetchContinueWithAuthAuthChallengeResponse)] ++ (catMaybes []))

data FetchGetResponseBody = FetchGetResponseBody {
    fetchGetResponseBodyBody :: String,
    fetchGetResponseBodyBase64Encoded :: Bool
} deriving Show
instance FromJSON  FetchGetResponseBody where
    parseJSON = A.withObject "FetchGetResponseBody" $ \v ->
         FetchGetResponseBody <$> v .:  "body"
            <*> v  .:  "base64Encoded"



fetchGetResponseBody :: Session -> FetchRequestId -> IO (Either Error FetchGetResponseBody)
fetchGetResponseBody session fetchGetResponseBodyRequestId = sendReceiveCommandResult (conn session) ("Fetch","getResponseBody") ([("fetchGetResponseBodyRequestId", ToJSONEx fetchGetResponseBodyRequestId)] ++ (catMaybes []))

data FetchTakeResponseBodyAsStream = FetchTakeResponseBodyAsStream {
    fetchTakeResponseBodyAsStreamStream :: IOStreamHandle
} deriving Show
instance FromJSON  FetchTakeResponseBodyAsStream where
    parseJSON = A.withObject "FetchTakeResponseBodyAsStream" $ \v ->
         FetchTakeResponseBodyAsStream <$> v .:  "stream"



fetchTakeResponseBodyAsStream :: Session -> FetchRequestId -> IO (Either Error FetchTakeResponseBodyAsStream)
fetchTakeResponseBodyAsStream session fetchTakeResponseBodyAsStreamRequestId = sendReceiveCommandResult (conn session) ("Fetch","takeResponseBodyAsStream") ([("fetchTakeResponseBodyAsStreamRequestId", ToJSONEx fetchTakeResponseBodyAsStreamRequestId)] ++ (catMaybes []))






data ConsoleConsoleMessage = ConsoleConsoleMessage {
    consoleConsoleMessageSource :: String,
    consoleConsoleMessageLevel :: String,
    consoleConsoleMessageText :: String,
    consoleConsoleMessageUrl :: Maybe String,
    consoleConsoleMessageLine :: Maybe Int,
    consoleConsoleMessageColumn :: Maybe Int
} deriving Show
instance FromJSON  ConsoleConsoleMessage where
    parseJSON = A.withObject "ConsoleConsoleMessage" $ \v ->
         ConsoleConsoleMessage <$> v .:  "source"
            <*> v  .:  "level"
            <*> v  .:  "text"
            <*> v  .:?  "url"
            <*> v  .:?  "line"
            <*> v  .:?  "column"


instance ToJSON ConsoleConsoleMessage  where
    toJSON v = A.object
        [ "source" .= consoleConsoleMessageSource v
        , "level" .= consoleConsoleMessageLevel v
        , "text" .= consoleConsoleMessageText v
        , "url" .= consoleConsoleMessageUrl v
        , "line" .= consoleConsoleMessageLine v
        , "column" .= consoleConsoleMessageColumn v
        ]



consoleClearMessages :: Session -> IO (Maybe Error)
consoleClearMessages session  = sendReceiveCommand (conn session) ("Console","clearMessages") ([] ++ (catMaybes []))


consoleDisable :: Session -> IO (Maybe Error)
consoleDisable session  = sendReceiveCommand (conn session) ("Console","disable") ([] ++ (catMaybes []))


consoleEnable :: Session -> IO (Maybe Error)
consoleEnable session  = sendReceiveCommand (conn session) ("Console","enable") ([] ++ (catMaybes []))



type DebuggerBreakpointId = String

type DebuggerCallFrameId = String

data DebuggerLocation = DebuggerLocation {
    debuggerLocationScriptId :: RuntimeScriptId,
    debuggerLocationLineNumber :: Int,
    debuggerLocationColumnNumber :: Maybe Int
} deriving Show
instance FromJSON  DebuggerLocation where
    parseJSON = A.withObject "DebuggerLocation" $ \v ->
         DebuggerLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"


instance ToJSON DebuggerLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerLocationScriptId v
        , "lineNumber" .= debuggerLocationLineNumber v
        , "columnNumber" .= debuggerLocationColumnNumber v
        ]



data DebuggerCallFrame = DebuggerCallFrame {
    debuggerCallFrameCallFrameId :: DebuggerCallFrameId,
    debuggerCallFrameFunctionName :: String,
    debuggerCallFrameLocation :: DebuggerLocation,
    debuggerCallFrameScopeChain :: [DebuggerScope],
    debuggerCallFrameThis :: RuntimeRemoteObject,
    debuggerCallFrameFunctionLocation :: Maybe DebuggerLocation,
    debuggerCallFrameReturnValue :: Maybe RuntimeRemoteObject
} deriving Show
instance FromJSON  DebuggerCallFrame where
    parseJSON = A.withObject "DebuggerCallFrame" $ \v ->
         DebuggerCallFrame <$> v .:  "callFrameId"
            <*> v  .:  "functionName"
            <*> v  .:  "location"
            <*> v  .:  "scopeChain"
            <*> v  .:  "this"
            <*> v  .:?  "functionLocation"
            <*> v  .:?  "returnValue"


instance ToJSON DebuggerCallFrame  where
    toJSON v = A.object
        [ "callFrameId" .= debuggerCallFrameCallFrameId v
        , "functionName" .= debuggerCallFrameFunctionName v
        , "location" .= debuggerCallFrameLocation v
        , "scopeChain" .= debuggerCallFrameScopeChain v
        , "this" .= debuggerCallFrameThis v
        , "functionLocation" .= debuggerCallFrameFunctionLocation v
        , "returnValue" .= debuggerCallFrameReturnValue v
        ]



data DebuggerScope = DebuggerScope {
    debuggerScopeType :: String,
    debuggerScopeObject :: RuntimeRemoteObject,
    debuggerScopeName :: Maybe String,
    debuggerScopeStartLocation :: Maybe DebuggerLocation,
    debuggerScopeEndLocation :: Maybe DebuggerLocation
} deriving Show
instance FromJSON  DebuggerScope where
    parseJSON = A.withObject "DebuggerScope" $ \v ->
         DebuggerScope <$> v .:  "type"
            <*> v  .:  "object"
            <*> v  .:?  "name"
            <*> v  .:?  "startLocation"
            <*> v  .:?  "endLocation"


instance ToJSON DebuggerScope  where
    toJSON v = A.object
        [ "type" .= debuggerScopeType v
        , "object" .= debuggerScopeObject v
        , "name" .= debuggerScopeName v
        , "startLocation" .= debuggerScopeStartLocation v
        , "endLocation" .= debuggerScopeEndLocation v
        ]



data DebuggerSearchMatch = DebuggerSearchMatch {
    debuggerSearchMatchLineNumber :: Int,
    debuggerSearchMatchLineContent :: String
} deriving Show
instance FromJSON  DebuggerSearchMatch where
    parseJSON = A.withObject "DebuggerSearchMatch" $ \v ->
         DebuggerSearchMatch <$> v .:  "lineNumber"
            <*> v  .:  "lineContent"


instance ToJSON DebuggerSearchMatch  where
    toJSON v = A.object
        [ "lineNumber" .= debuggerSearchMatchLineNumber v
        , "lineContent" .= debuggerSearchMatchLineContent v
        ]



data DebuggerBreakLocation = DebuggerBreakLocation {
    debuggerBreakLocationScriptId :: RuntimeScriptId,
    debuggerBreakLocationLineNumber :: Int,
    debuggerBreakLocationColumnNumber :: Maybe Int,
    debuggerBreakLocationType :: Maybe String
} deriving Show
instance FromJSON  DebuggerBreakLocation where
    parseJSON = A.withObject "DebuggerBreakLocation" $ \v ->
         DebuggerBreakLocation <$> v .:  "scriptId"
            <*> v  .:  "lineNumber"
            <*> v  .:?  "columnNumber"
            <*> v  .:?  "type"


instance ToJSON DebuggerBreakLocation  where
    toJSON v = A.object
        [ "scriptId" .= debuggerBreakLocationScriptId v
        , "lineNumber" .= debuggerBreakLocationLineNumber v
        , "columnNumber" .= debuggerBreakLocationColumnNumber v
        , "type" .= debuggerBreakLocationType v
        ]



data DebuggerScriptLanguage = DebuggerScriptLanguageJavaScript | DebuggerScriptLanguageWebAssembly
    deriving Show
instance FromJSON DebuggerScriptLanguage where
    parseJSON = A.withText  "DebuggerScriptLanguage"  $ \v -> 
        pure $ case v of
                "JavaScript" -> DebuggerScriptLanguageJavaScript
                "WebAssembly" -> DebuggerScriptLanguageWebAssembly

instance ToJSON DebuggerScriptLanguage where
    toJSON v = A.String $
        case v of
                DebuggerScriptLanguageJavaScript -> "JavaScript"
                DebuggerScriptLanguageWebAssembly -> "WebAssembly"



data DebuggerDebugSymbols = DebuggerDebugSymbols {
    debuggerDebugSymbolsType :: String,
    debuggerDebugSymbolsExternalUrl :: Maybe String
} deriving Show
instance FromJSON  DebuggerDebugSymbols where
    parseJSON = A.withObject "DebuggerDebugSymbols" $ \v ->
         DebuggerDebugSymbols <$> v .:  "type"
            <*> v  .:?  "externalURL"


instance ToJSON DebuggerDebugSymbols  where
    toJSON v = A.object
        [ "type" .= debuggerDebugSymbolsType v
        , "externalURL" .= debuggerDebugSymbolsExternalUrl v
        ]



debuggerContinueToLocation :: Session -> DebuggerLocation -> Maybe String -> IO (Maybe Error)
debuggerContinueToLocation session debuggerContinueToLocationLocation debuggerContinueToLocationTargetCallFrames = sendReceiveCommand (conn session) ("Debugger","continueToLocation") ([("debuggerContinueToLocationLocation", ToJSONEx debuggerContinueToLocationLocation)] ++ (catMaybes [fmap (("debuggerContinueToLocationTargetCallFrames",) . ToJSONEx) debuggerContinueToLocationTargetCallFrames]))


debuggerDisable :: Session -> IO (Maybe Error)
debuggerDisable session  = sendReceiveCommand (conn session) ("Debugger","disable") ([] ++ (catMaybes []))


debuggerEnable :: Session -> IO (Maybe Error)
debuggerEnable session  = sendReceiveCommand (conn session) ("Debugger","enable") ([] ++ (catMaybes []))

data DebuggerEvaluateOnCallFrame = DebuggerEvaluateOnCallFrame {
    debuggerEvaluateOnCallFrameResult :: RuntimeRemoteObject,
    debuggerEvaluateOnCallFrameExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  DebuggerEvaluateOnCallFrame where
    parseJSON = A.withObject "DebuggerEvaluateOnCallFrame" $ \v ->
         DebuggerEvaluateOnCallFrame <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



debuggerEvaluateOnCallFrame :: Session -> DebuggerCallFrameId -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error DebuggerEvaluateOnCallFrame)
debuggerEvaluateOnCallFrame session debuggerEvaluateOnCallFrameCallFrameId debuggerEvaluateOnCallFrameExpression debuggerEvaluateOnCallFrameObjectGroup debuggerEvaluateOnCallFrameIncludeCommandLineApi debuggerEvaluateOnCallFrameSilent debuggerEvaluateOnCallFrameReturnByValue debuggerEvaluateOnCallFrameThrowOnSideEffect = sendReceiveCommandResult (conn session) ("Debugger","evaluateOnCallFrame") ([("debuggerEvaluateOnCallFrameCallFrameId", ToJSONEx debuggerEvaluateOnCallFrameCallFrameId), ("debuggerEvaluateOnCallFrameExpression", ToJSONEx debuggerEvaluateOnCallFrameExpression)] ++ (catMaybes [fmap (("debuggerEvaluateOnCallFrameObjectGroup",) . ToJSONEx) debuggerEvaluateOnCallFrameObjectGroup, fmap (("debuggerEvaluateOnCallFrameIncludeCommandLineApi",) . ToJSONEx) debuggerEvaluateOnCallFrameIncludeCommandLineApi, fmap (("debuggerEvaluateOnCallFrameSilent",) . ToJSONEx) debuggerEvaluateOnCallFrameSilent, fmap (("debuggerEvaluateOnCallFrameReturnByValue",) . ToJSONEx) debuggerEvaluateOnCallFrameReturnByValue, fmap (("debuggerEvaluateOnCallFrameThrowOnSideEffect",) . ToJSONEx) debuggerEvaluateOnCallFrameThrowOnSideEffect]))

data DebuggerGetPossibleBreakpoints = DebuggerGetPossibleBreakpoints {
    debuggerGetPossibleBreakpointsLocations :: [DebuggerBreakLocation]
} deriving Show
instance FromJSON  DebuggerGetPossibleBreakpoints where
    parseJSON = A.withObject "DebuggerGetPossibleBreakpoints" $ \v ->
         DebuggerGetPossibleBreakpoints <$> v .:  "locations"



debuggerGetPossibleBreakpoints :: Session -> DebuggerLocation -> Maybe DebuggerLocation -> Maybe Bool -> IO (Either Error DebuggerGetPossibleBreakpoints)
debuggerGetPossibleBreakpoints session debuggerGetPossibleBreakpointsStart debuggerGetPossibleBreakpointsEnd debuggerGetPossibleBreakpointsRestrictToFunction = sendReceiveCommandResult (conn session) ("Debugger","getPossibleBreakpoints") ([("debuggerGetPossibleBreakpointsStart", ToJSONEx debuggerGetPossibleBreakpointsStart)] ++ (catMaybes [fmap (("debuggerGetPossibleBreakpointsEnd",) . ToJSONEx) debuggerGetPossibleBreakpointsEnd, fmap (("debuggerGetPossibleBreakpointsRestrictToFunction",) . ToJSONEx) debuggerGetPossibleBreakpointsRestrictToFunction]))

data DebuggerGetScriptSource = DebuggerGetScriptSource {
    debuggerGetScriptSourceScriptSource :: String,
    debuggerGetScriptSourceBytecode :: Maybe String
} deriving Show
instance FromJSON  DebuggerGetScriptSource where
    parseJSON = A.withObject "DebuggerGetScriptSource" $ \v ->
         DebuggerGetScriptSource <$> v .:  "scriptSource"
            <*> v  .:?  "bytecode"



debuggerGetScriptSource :: Session -> RuntimeScriptId -> IO (Either Error DebuggerGetScriptSource)
debuggerGetScriptSource session debuggerGetScriptSourceScriptId = sendReceiveCommandResult (conn session) ("Debugger","getScriptSource") ([("debuggerGetScriptSourceScriptId", ToJSONEx debuggerGetScriptSourceScriptId)] ++ (catMaybes []))


debuggerPause :: Session -> IO (Maybe Error)
debuggerPause session  = sendReceiveCommand (conn session) ("Debugger","pause") ([] ++ (catMaybes []))


debuggerRemoveBreakpoint :: Session -> DebuggerBreakpointId -> IO (Maybe Error)
debuggerRemoveBreakpoint session debuggerRemoveBreakpointBreakpointId = sendReceiveCommand (conn session) ("Debugger","removeBreakpoint") ([("debuggerRemoveBreakpointBreakpointId", ToJSONEx debuggerRemoveBreakpointBreakpointId)] ++ (catMaybes []))


debuggerResume :: Session -> Maybe Bool -> IO (Maybe Error)
debuggerResume session debuggerResumeTerminateOnResume = sendReceiveCommand (conn session) ("Debugger","resume") ([] ++ (catMaybes [fmap (("debuggerResumeTerminateOnResume",) . ToJSONEx) debuggerResumeTerminateOnResume]))

data DebuggerSearchInContent = DebuggerSearchInContent {
    debuggerSearchInContentResult :: [DebuggerSearchMatch]
} deriving Show
instance FromJSON  DebuggerSearchInContent where
    parseJSON = A.withObject "DebuggerSearchInContent" $ \v ->
         DebuggerSearchInContent <$> v .:  "result"



debuggerSearchInContent :: Session -> RuntimeScriptId -> String -> Maybe Bool -> Maybe Bool -> IO (Either Error DebuggerSearchInContent)
debuggerSearchInContent session debuggerSearchInContentScriptId debuggerSearchInContentQuery debuggerSearchInContentCaseSensitive debuggerSearchInContentIsRegex = sendReceiveCommandResult (conn session) ("Debugger","searchInContent") ([("debuggerSearchInContentScriptId", ToJSONEx debuggerSearchInContentScriptId), ("debuggerSearchInContentQuery", ToJSONEx debuggerSearchInContentQuery)] ++ (catMaybes [fmap (("debuggerSearchInContentCaseSensitive",) . ToJSONEx) debuggerSearchInContentCaseSensitive, fmap (("debuggerSearchInContentIsRegex",) . ToJSONEx) debuggerSearchInContentIsRegex]))


debuggerSetAsyncCallStackDepth :: Session -> Int -> IO (Maybe Error)
debuggerSetAsyncCallStackDepth session debuggerSetAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Debugger","setAsyncCallStackDepth") ([("debuggerSetAsyncCallStackDepthMaxDepth", ToJSONEx debuggerSetAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))

data DebuggerSetBreakpoint = DebuggerSetBreakpoint {
    debuggerSetBreakpointBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointActualLocation :: DebuggerLocation
} deriving Show
instance FromJSON  DebuggerSetBreakpoint where
    parseJSON = A.withObject "DebuggerSetBreakpoint" $ \v ->
         DebuggerSetBreakpoint <$> v .:  "breakpointId"
            <*> v  .:  "actualLocation"



debuggerSetBreakpoint :: Session -> DebuggerLocation -> Maybe String -> IO (Either Error DebuggerSetBreakpoint)
debuggerSetBreakpoint session debuggerSetBreakpointLocation debuggerSetBreakpointCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpoint") ([("debuggerSetBreakpointLocation", ToJSONEx debuggerSetBreakpointLocation)] ++ (catMaybes [fmap (("debuggerSetBreakpointCondition",) . ToJSONEx) debuggerSetBreakpointCondition]))

data DebuggerSetInstrumentationBreakpoint = DebuggerSetInstrumentationBreakpoint {
    debuggerSetInstrumentationBreakpointBreakpointId :: DebuggerBreakpointId
} deriving Show
instance FromJSON  DebuggerSetInstrumentationBreakpoint where
    parseJSON = A.withObject "DebuggerSetInstrumentationBreakpoint" $ \v ->
         DebuggerSetInstrumentationBreakpoint <$> v .:  "breakpointId"



debuggerSetInstrumentationBreakpoint :: Session -> String -> IO (Either Error DebuggerSetInstrumentationBreakpoint)
debuggerSetInstrumentationBreakpoint session debuggerSetInstrumentationBreakpointInstrumentation = sendReceiveCommandResult (conn session) ("Debugger","setInstrumentationBreakpoint") ([("debuggerSetInstrumentationBreakpointInstrumentation", ToJSONEx debuggerSetInstrumentationBreakpointInstrumentation)] ++ (catMaybes []))

data DebuggerSetBreakpointByUrl = DebuggerSetBreakpointByUrl {
    debuggerSetBreakpointByUrlBreakpointId :: DebuggerBreakpointId,
    debuggerSetBreakpointByUrlLocations :: [DebuggerLocation]
} deriving Show
instance FromJSON  DebuggerSetBreakpointByUrl where
    parseJSON = A.withObject "DebuggerSetBreakpointByUrl" $ \v ->
         DebuggerSetBreakpointByUrl <$> v .:  "breakpointId"
            <*> v  .:  "locations"



debuggerSetBreakpointByUrl :: Session -> Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe String -> IO (Either Error DebuggerSetBreakpointByUrl)
debuggerSetBreakpointByUrl session debuggerSetBreakpointByUrlLineNumber debuggerSetBreakpointByUrlUrl debuggerSetBreakpointByUrlUrlRegex debuggerSetBreakpointByUrlScriptHash debuggerSetBreakpointByUrlColumnNumber debuggerSetBreakpointByUrlCondition = sendReceiveCommandResult (conn session) ("Debugger","setBreakpointByUrl") ([("debuggerSetBreakpointByUrlLineNumber", ToJSONEx debuggerSetBreakpointByUrlLineNumber)] ++ (catMaybes [fmap (("debuggerSetBreakpointByUrlUrl",) . ToJSONEx) debuggerSetBreakpointByUrlUrl, fmap (("debuggerSetBreakpointByUrlUrlRegex",) . ToJSONEx) debuggerSetBreakpointByUrlUrlRegex, fmap (("debuggerSetBreakpointByUrlScriptHash",) . ToJSONEx) debuggerSetBreakpointByUrlScriptHash, fmap (("debuggerSetBreakpointByUrlColumnNumber",) . ToJSONEx) debuggerSetBreakpointByUrlColumnNumber, fmap (("debuggerSetBreakpointByUrlCondition",) . ToJSONEx) debuggerSetBreakpointByUrlCondition]))


debuggerSetBreakpointsActive :: Session -> Bool -> IO (Maybe Error)
debuggerSetBreakpointsActive session debuggerSetBreakpointsActiveActive = sendReceiveCommand (conn session) ("Debugger","setBreakpointsActive") ([("debuggerSetBreakpointsActiveActive", ToJSONEx debuggerSetBreakpointsActiveActive)] ++ (catMaybes []))


debuggerSetPauseOnExceptions :: Session -> String -> IO (Maybe Error)
debuggerSetPauseOnExceptions session debuggerSetPauseOnExceptionsState = sendReceiveCommand (conn session) ("Debugger","setPauseOnExceptions") ([("debuggerSetPauseOnExceptionsState", ToJSONEx debuggerSetPauseOnExceptionsState)] ++ (catMaybes []))

data DebuggerSetScriptSource = DebuggerSetScriptSource {
    debuggerSetScriptSourceCallFrames :: Maybe [DebuggerCallFrame],
    debuggerSetScriptSourceStackChanged :: Maybe Bool,
    debuggerSetScriptSourceAsyncStackTrace :: Maybe RuntimeStackTrace,
    debuggerSetScriptSourceExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  DebuggerSetScriptSource where
    parseJSON = A.withObject "DebuggerSetScriptSource" $ \v ->
         DebuggerSetScriptSource <$> v .:?  "callFrames"
            <*> v  .:?  "stackChanged"
            <*> v  .:?  "asyncStackTrace"
            <*> v  .:?  "exceptionDetails"



debuggerSetScriptSource :: Session -> RuntimeScriptId -> String -> Maybe Bool -> IO (Either Error DebuggerSetScriptSource)
debuggerSetScriptSource session debuggerSetScriptSourceScriptId debuggerSetScriptSourceScriptSource debuggerSetScriptSourceDryRun = sendReceiveCommandResult (conn session) ("Debugger","setScriptSource") ([("debuggerSetScriptSourceScriptId", ToJSONEx debuggerSetScriptSourceScriptId), ("debuggerSetScriptSourceScriptSource", ToJSONEx debuggerSetScriptSourceScriptSource)] ++ (catMaybes [fmap (("debuggerSetScriptSourceDryRun",) . ToJSONEx) debuggerSetScriptSourceDryRun]))


debuggerSetSkipAllPauses :: Session -> Bool -> IO (Maybe Error)
debuggerSetSkipAllPauses session debuggerSetSkipAllPausesSkip = sendReceiveCommand (conn session) ("Debugger","setSkipAllPauses") ([("debuggerSetSkipAllPausesSkip", ToJSONEx debuggerSetSkipAllPausesSkip)] ++ (catMaybes []))


debuggerSetVariableValue :: Session -> Int -> String -> RuntimeCallArgument -> DebuggerCallFrameId -> IO (Maybe Error)
debuggerSetVariableValue session debuggerSetVariableValueScopeNumber debuggerSetVariableValueVariableName debuggerSetVariableValueNewValue debuggerSetVariableValueCallFrameId = sendReceiveCommand (conn session) ("Debugger","setVariableValue") ([("debuggerSetVariableValueScopeNumber", ToJSONEx debuggerSetVariableValueScopeNumber), ("debuggerSetVariableValueVariableName", ToJSONEx debuggerSetVariableValueVariableName), ("debuggerSetVariableValueNewValue", ToJSONEx debuggerSetVariableValueNewValue), ("debuggerSetVariableValueCallFrameId", ToJSONEx debuggerSetVariableValueCallFrameId)] ++ (catMaybes []))


debuggerStepInto :: Session -> IO (Maybe Error)
debuggerStepInto session  = sendReceiveCommand (conn session) ("Debugger","stepInto") ([] ++ (catMaybes []))


debuggerStepOut :: Session -> IO (Maybe Error)
debuggerStepOut session  = sendReceiveCommand (conn session) ("Debugger","stepOut") ([] ++ (catMaybes []))


debuggerStepOver :: Session -> IO (Maybe Error)
debuggerStepOver session  = sendReceiveCommand (conn session) ("Debugger","stepOver") ([] ++ (catMaybes []))



data ProfilerProfileNode = ProfilerProfileNode {
    profilerProfileNodeId :: Int,
    profilerProfileNodeCallFrame :: RuntimeCallFrame,
    profilerProfileNodeHitCount :: Maybe Int,
    profilerProfileNodeChildren :: Maybe [Int],
    profilerProfileNodeDeoptReason :: Maybe String,
    profilerProfileNodePositionTicks :: Maybe [ProfilerPositionTickInfo]
} deriving Show
instance FromJSON  ProfilerProfileNode where
    parseJSON = A.withObject "ProfilerProfileNode" $ \v ->
         ProfilerProfileNode <$> v .:  "id"
            <*> v  .:  "callFrame"
            <*> v  .:?  "hitCount"
            <*> v  .:?  "children"
            <*> v  .:?  "deoptReason"
            <*> v  .:?  "positionTicks"


instance ToJSON ProfilerProfileNode  where
    toJSON v = A.object
        [ "id" .= profilerProfileNodeId v
        , "callFrame" .= profilerProfileNodeCallFrame v
        , "hitCount" .= profilerProfileNodeHitCount v
        , "children" .= profilerProfileNodeChildren v
        , "deoptReason" .= profilerProfileNodeDeoptReason v
        , "positionTicks" .= profilerProfileNodePositionTicks v
        ]



data ProfilerProfile = ProfilerProfile {
    profilerProfileNodes :: [ProfilerProfileNode],
    profilerProfileStartTime :: Int,
    profilerProfileEndTime :: Int,
    profilerProfileSamples :: Maybe [Int],
    profilerProfileTimeDeltas :: Maybe [Int]
} deriving Show
instance FromJSON  ProfilerProfile where
    parseJSON = A.withObject "ProfilerProfile" $ \v ->
         ProfilerProfile <$> v .:  "nodes"
            <*> v  .:  "startTime"
            <*> v  .:  "endTime"
            <*> v  .:?  "samples"
            <*> v  .:?  "timeDeltas"


instance ToJSON ProfilerProfile  where
    toJSON v = A.object
        [ "nodes" .= profilerProfileNodes v
        , "startTime" .= profilerProfileStartTime v
        , "endTime" .= profilerProfileEndTime v
        , "samples" .= profilerProfileSamples v
        , "timeDeltas" .= profilerProfileTimeDeltas v
        ]



data ProfilerPositionTickInfo = ProfilerPositionTickInfo {
    profilerPositionTickInfoLine :: Int,
    profilerPositionTickInfoTicks :: Int
} deriving Show
instance FromJSON  ProfilerPositionTickInfo where
    parseJSON = A.withObject "ProfilerPositionTickInfo" $ \v ->
         ProfilerPositionTickInfo <$> v .:  "line"
            <*> v  .:  "ticks"


instance ToJSON ProfilerPositionTickInfo  where
    toJSON v = A.object
        [ "line" .= profilerPositionTickInfoLine v
        , "ticks" .= profilerPositionTickInfoTicks v
        ]



data ProfilerCoverageRange = ProfilerCoverageRange {
    profilerCoverageRangeStartOffset :: Int,
    profilerCoverageRangeEndOffset :: Int,
    profilerCoverageRangeCount :: Int
} deriving Show
instance FromJSON  ProfilerCoverageRange where
    parseJSON = A.withObject "ProfilerCoverageRange" $ \v ->
         ProfilerCoverageRange <$> v .:  "startOffset"
            <*> v  .:  "endOffset"
            <*> v  .:  "count"


instance ToJSON ProfilerCoverageRange  where
    toJSON v = A.object
        [ "startOffset" .= profilerCoverageRangeStartOffset v
        , "endOffset" .= profilerCoverageRangeEndOffset v
        , "count" .= profilerCoverageRangeCount v
        ]



data ProfilerFunctionCoverage = ProfilerFunctionCoverage {
    profilerFunctionCoverageFunctionName :: String,
    profilerFunctionCoverageRanges :: [ProfilerCoverageRange],
    profilerFunctionCoverageIsBlockCoverage :: Bool
} deriving Show
instance FromJSON  ProfilerFunctionCoverage where
    parseJSON = A.withObject "ProfilerFunctionCoverage" $ \v ->
         ProfilerFunctionCoverage <$> v .:  "functionName"
            <*> v  .:  "ranges"
            <*> v  .:  "isBlockCoverage"


instance ToJSON ProfilerFunctionCoverage  where
    toJSON v = A.object
        [ "functionName" .= profilerFunctionCoverageFunctionName v
        , "ranges" .= profilerFunctionCoverageRanges v
        , "isBlockCoverage" .= profilerFunctionCoverageIsBlockCoverage v
        ]



data ProfilerScriptCoverage = ProfilerScriptCoverage {
    profilerScriptCoverageScriptId :: RuntimeScriptId,
    profilerScriptCoverageUrl :: String,
    profilerScriptCoverageFunctions :: [ProfilerFunctionCoverage]
} deriving Show
instance FromJSON  ProfilerScriptCoverage where
    parseJSON = A.withObject "ProfilerScriptCoverage" $ \v ->
         ProfilerScriptCoverage <$> v .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "functions"


instance ToJSON ProfilerScriptCoverage  where
    toJSON v = A.object
        [ "scriptId" .= profilerScriptCoverageScriptId v
        , "url" .= profilerScriptCoverageUrl v
        , "functions" .= profilerScriptCoverageFunctions v
        ]



profilerDisable :: Session -> IO (Maybe Error)
profilerDisable session  = sendReceiveCommand (conn session) ("Profiler","disable") ([] ++ (catMaybes []))


profilerEnable :: Session -> IO (Maybe Error)
profilerEnable session  = sendReceiveCommand (conn session) ("Profiler","enable") ([] ++ (catMaybes []))

data ProfilerGetBestEffortCoverage = ProfilerGetBestEffortCoverage {
    profilerGetBestEffortCoverageResult :: [ProfilerScriptCoverage]
} deriving Show
instance FromJSON  ProfilerGetBestEffortCoverage where
    parseJSON = A.withObject "ProfilerGetBestEffortCoverage" $ \v ->
         ProfilerGetBestEffortCoverage <$> v .:  "result"



profilerGetBestEffortCoverage :: Session -> IO (Either Error ProfilerGetBestEffortCoverage)
profilerGetBestEffortCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","getBestEffortCoverage") ([] ++ (catMaybes []))


profilerSetSamplingInterval :: Session -> Int -> IO (Maybe Error)
profilerSetSamplingInterval session profilerSetSamplingIntervalInterval = sendReceiveCommand (conn session) ("Profiler","setSamplingInterval") ([("profilerSetSamplingIntervalInterval", ToJSONEx profilerSetSamplingIntervalInterval)] ++ (catMaybes []))


profilerStart :: Session -> IO (Maybe Error)
profilerStart session  = sendReceiveCommand (conn session) ("Profiler","start") ([] ++ (catMaybes []))

data ProfilerStartPreciseCoverage = ProfilerStartPreciseCoverage {
    profilerStartPreciseCoverageTimestamp :: Int
} deriving Show
instance FromJSON  ProfilerStartPreciseCoverage where
    parseJSON = A.withObject "ProfilerStartPreciseCoverage" $ \v ->
         ProfilerStartPreciseCoverage <$> v .:  "timestamp"



profilerStartPreciseCoverage :: Session -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error ProfilerStartPreciseCoverage)
profilerStartPreciseCoverage session profilerStartPreciseCoverageCallCount profilerStartPreciseCoverageDetailed profilerStartPreciseCoverageAllowTriggeredUpdates = sendReceiveCommandResult (conn session) ("Profiler","startPreciseCoverage") ([] ++ (catMaybes [fmap (("profilerStartPreciseCoverageCallCount",) . ToJSONEx) profilerStartPreciseCoverageCallCount, fmap (("profilerStartPreciseCoverageDetailed",) . ToJSONEx) profilerStartPreciseCoverageDetailed, fmap (("profilerStartPreciseCoverageAllowTriggeredUpdates",) . ToJSONEx) profilerStartPreciseCoverageAllowTriggeredUpdates]))

data ProfilerStop = ProfilerStop {
    profilerStopProfile :: ProfilerProfile
} deriving Show
instance FromJSON  ProfilerStop where
    parseJSON = A.withObject "ProfilerStop" $ \v ->
         ProfilerStop <$> v .:  "profile"



profilerStop :: Session -> IO (Either Error ProfilerStop)
profilerStop session  = sendReceiveCommandResult (conn session) ("Profiler","stop") ([] ++ (catMaybes []))


profilerStopPreciseCoverage :: Session -> IO (Maybe Error)
profilerStopPreciseCoverage session  = sendReceiveCommand (conn session) ("Profiler","stopPreciseCoverage") ([] ++ (catMaybes []))

data ProfilerTakePreciseCoverage = ProfilerTakePreciseCoverage {
    profilerTakePreciseCoverageResult :: [ProfilerScriptCoverage],
    profilerTakePreciseCoverageTimestamp :: Int
} deriving Show
instance FromJSON  ProfilerTakePreciseCoverage where
    parseJSON = A.withObject "ProfilerTakePreciseCoverage" $ \v ->
         ProfilerTakePreciseCoverage <$> v .:  "result"
            <*> v  .:  "timestamp"



profilerTakePreciseCoverage :: Session -> IO (Either Error ProfilerTakePreciseCoverage)
profilerTakePreciseCoverage session  = sendReceiveCommandResult (conn session) ("Profiler","takePreciseCoverage") ([] ++ (catMaybes []))



type RuntimeScriptId = String

data RuntimeWebDriverValue = RuntimeWebDriverValue {
    runtimeWebDriverValueType :: String,
    runtimeWebDriverValueValue :: Maybe Int,
    runtimeWebDriverValueObjectId :: Maybe String
} deriving Show
instance FromJSON  RuntimeWebDriverValue where
    parseJSON = A.withObject "RuntimeWebDriverValue" $ \v ->
         RuntimeWebDriverValue <$> v .:  "type"
            <*> v  .:?  "value"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeWebDriverValue  where
    toJSON v = A.object
        [ "type" .= runtimeWebDriverValueType v
        , "value" .= runtimeWebDriverValueValue v
        , "objectId" .= runtimeWebDriverValueObjectId v
        ]



type RuntimeRemoteObjectId = String

type RuntimeUnserializableValue = String

data RuntimeRemoteObject = RuntimeRemoteObject {
    runtimeRemoteObjectType :: String,
    runtimeRemoteObjectSubtype :: Maybe String,
    runtimeRemoteObjectClassName :: Maybe String,
    runtimeRemoteObjectValue :: Maybe Int,
    runtimeRemoteObjectUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeRemoteObjectDescription :: Maybe String,
    runtimeRemoteObjectObjectId :: Maybe RuntimeRemoteObjectId
} deriving Show
instance FromJSON  RuntimeRemoteObject where
    parseJSON = A.withObject "RuntimeRemoteObject" $ \v ->
         RuntimeRemoteObject <$> v .:  "type"
            <*> v  .:?  "subtype"
            <*> v  .:?  "className"
            <*> v  .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "description"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeRemoteObject  where
    toJSON v = A.object
        [ "type" .= runtimeRemoteObjectType v
        , "subtype" .= runtimeRemoteObjectSubtype v
        , "className" .= runtimeRemoteObjectClassName v
        , "value" .= runtimeRemoteObjectValue v
        , "unserializableValue" .= runtimeRemoteObjectUnserializableValue v
        , "description" .= runtimeRemoteObjectDescription v
        , "objectId" .= runtimeRemoteObjectObjectId v
        ]



data RuntimePropertyDescriptor = RuntimePropertyDescriptor {
    runtimePropertyDescriptorName :: String,
    runtimePropertyDescriptorConfigurable :: Bool,
    runtimePropertyDescriptorEnumerable :: Bool,
    runtimePropertyDescriptorValue :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWritable :: Maybe Bool,
    runtimePropertyDescriptorGet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorSet :: Maybe RuntimeRemoteObject,
    runtimePropertyDescriptorWasThrown :: Maybe Bool,
    runtimePropertyDescriptorIsOwn :: Maybe Bool,
    runtimePropertyDescriptorSymbol :: Maybe RuntimeRemoteObject
} deriving Show
instance FromJSON  RuntimePropertyDescriptor where
    parseJSON = A.withObject "RuntimePropertyDescriptor" $ \v ->
         RuntimePropertyDescriptor <$> v .:  "name"
            <*> v  .:  "configurable"
            <*> v  .:  "enumerable"
            <*> v  .:?  "value"
            <*> v  .:?  "writable"
            <*> v  .:?  "get"
            <*> v  .:?  "set"
            <*> v  .:?  "wasThrown"
            <*> v  .:?  "isOwn"
            <*> v  .:?  "symbol"


instance ToJSON RuntimePropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimePropertyDescriptorName v
        , "configurable" .= runtimePropertyDescriptorConfigurable v
        , "enumerable" .= runtimePropertyDescriptorEnumerable v
        , "value" .= runtimePropertyDescriptorValue v
        , "writable" .= runtimePropertyDescriptorWritable v
        , "get" .= runtimePropertyDescriptorGet v
        , "set" .= runtimePropertyDescriptorSet v
        , "wasThrown" .= runtimePropertyDescriptorWasThrown v
        , "isOwn" .= runtimePropertyDescriptorIsOwn v
        , "symbol" .= runtimePropertyDescriptorSymbol v
        ]



data RuntimeInternalPropertyDescriptor = RuntimeInternalPropertyDescriptor {
    runtimeInternalPropertyDescriptorName :: String,
    runtimeInternalPropertyDescriptorValue :: Maybe RuntimeRemoteObject
} deriving Show
instance FromJSON  RuntimeInternalPropertyDescriptor where
    parseJSON = A.withObject "RuntimeInternalPropertyDescriptor" $ \v ->
         RuntimeInternalPropertyDescriptor <$> v .:  "name"
            <*> v  .:?  "value"


instance ToJSON RuntimeInternalPropertyDescriptor  where
    toJSON v = A.object
        [ "name" .= runtimeInternalPropertyDescriptorName v
        , "value" .= runtimeInternalPropertyDescriptorValue v
        ]



data RuntimeCallArgument = RuntimeCallArgument {
    runtimeCallArgumentValue :: Maybe Int,
    runtimeCallArgumentUnserializableValue :: Maybe RuntimeUnserializableValue,
    runtimeCallArgumentObjectId :: Maybe RuntimeRemoteObjectId
} deriving Show
instance FromJSON  RuntimeCallArgument where
    parseJSON = A.withObject "RuntimeCallArgument" $ \v ->
         RuntimeCallArgument <$> v .:?  "value"
            <*> v  .:?  "unserializableValue"
            <*> v  .:?  "objectId"


instance ToJSON RuntimeCallArgument  where
    toJSON v = A.object
        [ "value" .= runtimeCallArgumentValue v
        , "unserializableValue" .= runtimeCallArgumentUnserializableValue v
        , "objectId" .= runtimeCallArgumentObjectId v
        ]



type RuntimeExecutionContextId = Int

data RuntimeExecutionContextDescription = RuntimeExecutionContextDescription {
    runtimeExecutionContextDescriptionId :: RuntimeExecutionContextId,
    runtimeExecutionContextDescriptionOrigin :: String,
    runtimeExecutionContextDescriptionName :: String,
    runtimeExecutionContextDescriptionAuxData :: Maybe [(String, String)]
} deriving Show
instance FromJSON  RuntimeExecutionContextDescription where
    parseJSON = A.withObject "RuntimeExecutionContextDescription" $ \v ->
         RuntimeExecutionContextDescription <$> v .:  "id"
            <*> v  .:  "origin"
            <*> v  .:  "name"
            <*> v  .:?  "auxData"


instance ToJSON RuntimeExecutionContextDescription  where
    toJSON v = A.object
        [ "id" .= runtimeExecutionContextDescriptionId v
        , "origin" .= runtimeExecutionContextDescriptionOrigin v
        , "name" .= runtimeExecutionContextDescriptionName v
        , "auxData" .= runtimeExecutionContextDescriptionAuxData v
        ]



data RuntimeExceptionDetails = RuntimeExceptionDetails {
    runtimeExceptionDetailsExceptionId :: Int,
    runtimeExceptionDetailsText :: String,
    runtimeExceptionDetailsLineNumber :: Int,
    runtimeExceptionDetailsColumnNumber :: Int,
    runtimeExceptionDetailsScriptId :: Maybe RuntimeScriptId,
    runtimeExceptionDetailsUrl :: Maybe String,
    runtimeExceptionDetailsStackTrace :: Maybe RuntimeStackTrace,
    runtimeExceptionDetailsException :: Maybe RuntimeRemoteObject,
    runtimeExceptionDetailsExecutionContextId :: Maybe RuntimeExecutionContextId
} deriving Show
instance FromJSON  RuntimeExceptionDetails where
    parseJSON = A.withObject "RuntimeExceptionDetails" $ \v ->
         RuntimeExceptionDetails <$> v .:  "exceptionId"
            <*> v  .:  "text"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"
            <*> v  .:?  "scriptId"
            <*> v  .:?  "url"
            <*> v  .:?  "stackTrace"
            <*> v  .:?  "exception"
            <*> v  .:?  "executionContextId"


instance ToJSON RuntimeExceptionDetails  where
    toJSON v = A.object
        [ "exceptionId" .= runtimeExceptionDetailsExceptionId v
        , "text" .= runtimeExceptionDetailsText v
        , "lineNumber" .= runtimeExceptionDetailsLineNumber v
        , "columnNumber" .= runtimeExceptionDetailsColumnNumber v
        , "scriptId" .= runtimeExceptionDetailsScriptId v
        , "url" .= runtimeExceptionDetailsUrl v
        , "stackTrace" .= runtimeExceptionDetailsStackTrace v
        , "exception" .= runtimeExceptionDetailsException v
        , "executionContextId" .= runtimeExceptionDetailsExecutionContextId v
        ]



type RuntimeTimestamp = Int

type RuntimeTimeDelta = Int

data RuntimeCallFrame = RuntimeCallFrame {
    runtimeCallFrameFunctionName :: String,
    runtimeCallFrameScriptId :: RuntimeScriptId,
    runtimeCallFrameUrl :: String,
    runtimeCallFrameLineNumber :: Int,
    runtimeCallFrameColumnNumber :: Int
} deriving Show
instance FromJSON  RuntimeCallFrame where
    parseJSON = A.withObject "RuntimeCallFrame" $ \v ->
         RuntimeCallFrame <$> v .:  "functionName"
            <*> v  .:  "scriptId"
            <*> v  .:  "url"
            <*> v  .:  "lineNumber"
            <*> v  .:  "columnNumber"


instance ToJSON RuntimeCallFrame  where
    toJSON v = A.object
        [ "functionName" .= runtimeCallFrameFunctionName v
        , "scriptId" .= runtimeCallFrameScriptId v
        , "url" .= runtimeCallFrameUrl v
        , "lineNumber" .= runtimeCallFrameLineNumber v
        , "columnNumber" .= runtimeCallFrameColumnNumber v
        ]



data RuntimeStackTrace = RuntimeStackTrace {
    runtimeStackTraceCallFrames :: [RuntimeCallFrame],
    runtimeStackTraceDescription :: Maybe String,
    runtimeStackTraceParent :: Maybe RuntimeStackTrace
} deriving Show
instance FromJSON  RuntimeStackTrace where
    parseJSON = A.withObject "RuntimeStackTrace" $ \v ->
         RuntimeStackTrace <$> v .:  "callFrames"
            <*> v  .:?  "description"
            <*> v  .:?  "parent"


instance ToJSON RuntimeStackTrace  where
    toJSON v = A.object
        [ "callFrames" .= runtimeStackTraceCallFrames v
        , "description" .= runtimeStackTraceDescription v
        , "parent" .= runtimeStackTraceParent v
        ]


data RuntimeAwaitPromise = RuntimeAwaitPromise {
    runtimeAwaitPromiseResult :: RuntimeRemoteObject,
    runtimeAwaitPromiseExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeAwaitPromise where
    parseJSON = A.withObject "RuntimeAwaitPromise" $ \v ->
         RuntimeAwaitPromise <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeAwaitPromise :: Session -> RuntimeRemoteObjectId -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeAwaitPromise)
runtimeAwaitPromise session runtimeAwaitPromisePromiseObjectId runtimeAwaitPromiseReturnByValue runtimeAwaitPromiseGeneratePreview = sendReceiveCommandResult (conn session) ("Runtime","awaitPromise") ([("runtimeAwaitPromisePromiseObjectId", ToJSONEx runtimeAwaitPromisePromiseObjectId)] ++ (catMaybes [fmap (("runtimeAwaitPromiseReturnByValue",) . ToJSONEx) runtimeAwaitPromiseReturnByValue, fmap (("runtimeAwaitPromiseGeneratePreview",) . ToJSONEx) runtimeAwaitPromiseGeneratePreview]))

data RuntimeCallFunctionOn = RuntimeCallFunctionOn {
    runtimeCallFunctionOnResult :: RuntimeRemoteObject,
    runtimeCallFunctionOnExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeCallFunctionOn where
    parseJSON = A.withObject "RuntimeCallFunctionOn" $ \v ->
         RuntimeCallFunctionOn <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeCallFunctionOn :: Session -> String -> Maybe RuntimeRemoteObjectId -> Maybe [RuntimeCallArgument] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe RuntimeExecutionContextId -> Maybe String -> IO (Either Error RuntimeCallFunctionOn)
runtimeCallFunctionOn session runtimeCallFunctionOnFunctionDeclaration runtimeCallFunctionOnObjectId runtimeCallFunctionOnArguments runtimeCallFunctionOnSilent runtimeCallFunctionOnReturnByValue runtimeCallFunctionOnUserGesture runtimeCallFunctionOnAwaitPromise runtimeCallFunctionOnExecutionContextId runtimeCallFunctionOnObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","callFunctionOn") ([("runtimeCallFunctionOnFunctionDeclaration", ToJSONEx runtimeCallFunctionOnFunctionDeclaration)] ++ (catMaybes [fmap (("runtimeCallFunctionOnObjectId",) . ToJSONEx) runtimeCallFunctionOnObjectId, fmap (("runtimeCallFunctionOnArguments",) . ToJSONEx) runtimeCallFunctionOnArguments, fmap (("runtimeCallFunctionOnSilent",) . ToJSONEx) runtimeCallFunctionOnSilent, fmap (("runtimeCallFunctionOnReturnByValue",) . ToJSONEx) runtimeCallFunctionOnReturnByValue, fmap (("runtimeCallFunctionOnUserGesture",) . ToJSONEx) runtimeCallFunctionOnUserGesture, fmap (("runtimeCallFunctionOnAwaitPromise",) . ToJSONEx) runtimeCallFunctionOnAwaitPromise, fmap (("runtimeCallFunctionOnExecutionContextId",) . ToJSONEx) runtimeCallFunctionOnExecutionContextId, fmap (("runtimeCallFunctionOnObjectGroup",) . ToJSONEx) runtimeCallFunctionOnObjectGroup]))

data RuntimeCompileScript = RuntimeCompileScript {
    runtimeCompileScriptScriptId :: Maybe RuntimeScriptId,
    runtimeCompileScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeCompileScript where
    parseJSON = A.withObject "RuntimeCompileScript" $ \v ->
         RuntimeCompileScript <$> v .:?  "scriptId"
            <*> v  .:?  "exceptionDetails"



runtimeCompileScript :: Session -> String -> String -> Bool -> Maybe RuntimeExecutionContextId -> IO (Either Error RuntimeCompileScript)
runtimeCompileScript session runtimeCompileScriptExpression runtimeCompileScriptSourceUrl runtimeCompileScriptPersistScript runtimeCompileScriptExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","compileScript") ([("runtimeCompileScriptExpression", ToJSONEx runtimeCompileScriptExpression), ("runtimeCompileScriptSourceUrl", ToJSONEx runtimeCompileScriptSourceUrl), ("runtimeCompileScriptPersistScript", ToJSONEx runtimeCompileScriptPersistScript)] ++ (catMaybes [fmap (("runtimeCompileScriptExecutionContextId",) . ToJSONEx) runtimeCompileScriptExecutionContextId]))


runtimeDisable :: Session -> IO (Maybe Error)
runtimeDisable session  = sendReceiveCommand (conn session) ("Runtime","disable") ([] ++ (catMaybes []))


runtimeDiscardConsoleEntries :: Session -> IO (Maybe Error)
runtimeDiscardConsoleEntries session  = sendReceiveCommand (conn session) ("Runtime","discardConsoleEntries") ([] ++ (catMaybes []))


runtimeEnable :: Session -> IO (Maybe Error)
runtimeEnable session  = sendReceiveCommand (conn session) ("Runtime","enable") ([] ++ (catMaybes []))

data RuntimeEvaluate = RuntimeEvaluate {
    runtimeEvaluateResult :: RuntimeRemoteObject,
    runtimeEvaluateExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeEvaluate where
    parseJSON = A.withObject "RuntimeEvaluate" $ \v ->
         RuntimeEvaluate <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeEvaluate :: Session -> String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe RuntimeExecutionContextId -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeEvaluate)
runtimeEvaluate session runtimeEvaluateExpression runtimeEvaluateObjectGroup runtimeEvaluateIncludeCommandLineApi runtimeEvaluateSilent runtimeEvaluateContextId runtimeEvaluateReturnByValue runtimeEvaluateUserGesture runtimeEvaluateAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","evaluate") ([("runtimeEvaluateExpression", ToJSONEx runtimeEvaluateExpression)] ++ (catMaybes [fmap (("runtimeEvaluateObjectGroup",) . ToJSONEx) runtimeEvaluateObjectGroup, fmap (("runtimeEvaluateIncludeCommandLineApi",) . ToJSONEx) runtimeEvaluateIncludeCommandLineApi, fmap (("runtimeEvaluateSilent",) . ToJSONEx) runtimeEvaluateSilent, fmap (("runtimeEvaluateContextId",) . ToJSONEx) runtimeEvaluateContextId, fmap (("runtimeEvaluateReturnByValue",) . ToJSONEx) runtimeEvaluateReturnByValue, fmap (("runtimeEvaluateUserGesture",) . ToJSONEx) runtimeEvaluateUserGesture, fmap (("runtimeEvaluateAwaitPromise",) . ToJSONEx) runtimeEvaluateAwaitPromise]))

data RuntimeGetProperties = RuntimeGetProperties {
    runtimeGetPropertiesResult :: [RuntimePropertyDescriptor],
    runtimeGetPropertiesInternalProperties :: Maybe [RuntimeInternalPropertyDescriptor],
    runtimeGetPropertiesExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeGetProperties where
    parseJSON = A.withObject "RuntimeGetProperties" $ \v ->
         RuntimeGetProperties <$> v .:  "result"
            <*> v  .:?  "internalProperties"
            <*> v  .:?  "exceptionDetails"



runtimeGetProperties :: Session -> RuntimeRemoteObjectId -> Maybe Bool -> IO (Either Error RuntimeGetProperties)
runtimeGetProperties session runtimeGetPropertiesObjectId runtimeGetPropertiesOwnProperties = sendReceiveCommandResult (conn session) ("Runtime","getProperties") ([("runtimeGetPropertiesObjectId", ToJSONEx runtimeGetPropertiesObjectId)] ++ (catMaybes [fmap (("runtimeGetPropertiesOwnProperties",) . ToJSONEx) runtimeGetPropertiesOwnProperties]))

data RuntimeGlobalLexicalScopeNames = RuntimeGlobalLexicalScopeNames {
    runtimeGlobalLexicalScopeNamesNames :: [String]
} deriving Show
instance FromJSON  RuntimeGlobalLexicalScopeNames where
    parseJSON = A.withObject "RuntimeGlobalLexicalScopeNames" $ \v ->
         RuntimeGlobalLexicalScopeNames <$> v .:  "names"



runtimeGlobalLexicalScopeNames :: Session -> Maybe RuntimeExecutionContextId -> IO (Either Error RuntimeGlobalLexicalScopeNames)
runtimeGlobalLexicalScopeNames session runtimeGlobalLexicalScopeNamesExecutionContextId = sendReceiveCommandResult (conn session) ("Runtime","globalLexicalScopeNames") ([] ++ (catMaybes [fmap (("runtimeGlobalLexicalScopeNamesExecutionContextId",) . ToJSONEx) runtimeGlobalLexicalScopeNamesExecutionContextId]))

data RuntimeQueryObjects = RuntimeQueryObjects {
    runtimeQueryObjectsObjects :: RuntimeRemoteObject
} deriving Show
instance FromJSON  RuntimeQueryObjects where
    parseJSON = A.withObject "RuntimeQueryObjects" $ \v ->
         RuntimeQueryObjects <$> v .:  "objects"



runtimeQueryObjects :: Session -> RuntimeRemoteObjectId -> Maybe String -> IO (Either Error RuntimeQueryObjects)
runtimeQueryObjects session runtimeQueryObjectsPrototypeObjectId runtimeQueryObjectsObjectGroup = sendReceiveCommandResult (conn session) ("Runtime","queryObjects") ([("runtimeQueryObjectsPrototypeObjectId", ToJSONEx runtimeQueryObjectsPrototypeObjectId)] ++ (catMaybes [fmap (("runtimeQueryObjectsObjectGroup",) . ToJSONEx) runtimeQueryObjectsObjectGroup]))


runtimeReleaseObject :: Session -> RuntimeRemoteObjectId -> IO (Maybe Error)
runtimeReleaseObject session runtimeReleaseObjectObjectId = sendReceiveCommand (conn session) ("Runtime","releaseObject") ([("runtimeReleaseObjectObjectId", ToJSONEx runtimeReleaseObjectObjectId)] ++ (catMaybes []))


runtimeReleaseObjectGroup :: Session -> String -> IO (Maybe Error)
runtimeReleaseObjectGroup session runtimeReleaseObjectGroupObjectGroup = sendReceiveCommand (conn session) ("Runtime","releaseObjectGroup") ([("runtimeReleaseObjectGroupObjectGroup", ToJSONEx runtimeReleaseObjectGroupObjectGroup)] ++ (catMaybes []))


runtimeRunIfWaitingForDebugger :: Session -> IO (Maybe Error)
runtimeRunIfWaitingForDebugger session  = sendReceiveCommand (conn session) ("Runtime","runIfWaitingForDebugger") ([] ++ (catMaybes []))

data RuntimeRunScript = RuntimeRunScript {
    runtimeRunScriptResult :: RuntimeRemoteObject,
    runtimeRunScriptExceptionDetails :: Maybe RuntimeExceptionDetails
} deriving Show
instance FromJSON  RuntimeRunScript where
    parseJSON = A.withObject "RuntimeRunScript" $ \v ->
         RuntimeRunScript <$> v .:  "result"
            <*> v  .:?  "exceptionDetails"



runtimeRunScript :: Session -> RuntimeScriptId -> Maybe RuntimeExecutionContextId -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO (Either Error RuntimeRunScript)
runtimeRunScript session runtimeRunScriptScriptId runtimeRunScriptExecutionContextId runtimeRunScriptObjectGroup runtimeRunScriptSilent runtimeRunScriptIncludeCommandLineApi runtimeRunScriptReturnByValue runtimeRunScriptGeneratePreview runtimeRunScriptAwaitPromise = sendReceiveCommandResult (conn session) ("Runtime","runScript") ([("runtimeRunScriptScriptId", ToJSONEx runtimeRunScriptScriptId)] ++ (catMaybes [fmap (("runtimeRunScriptExecutionContextId",) . ToJSONEx) runtimeRunScriptExecutionContextId, fmap (("runtimeRunScriptObjectGroup",) . ToJSONEx) runtimeRunScriptObjectGroup, fmap (("runtimeRunScriptSilent",) . ToJSONEx) runtimeRunScriptSilent, fmap (("runtimeRunScriptIncludeCommandLineApi",) . ToJSONEx) runtimeRunScriptIncludeCommandLineApi, fmap (("runtimeRunScriptReturnByValue",) . ToJSONEx) runtimeRunScriptReturnByValue, fmap (("runtimeRunScriptGeneratePreview",) . ToJSONEx) runtimeRunScriptGeneratePreview, fmap (("runtimeRunScriptAwaitPromise",) . ToJSONEx) runtimeRunScriptAwaitPromise]))


runtimeSetAsyncCallStackDepth :: Session -> Int -> IO (Maybe Error)
runtimeSetAsyncCallStackDepth session runtimeSetAsyncCallStackDepthMaxDepth = sendReceiveCommand (conn session) ("Runtime","setAsyncCallStackDepth") ([("runtimeSetAsyncCallStackDepthMaxDepth", ToJSONEx runtimeSetAsyncCallStackDepthMaxDepth)] ++ (catMaybes []))



data SchemaDomain = SchemaDomain {
    schemaDomainName :: String,
    schemaDomainVersion :: String
} deriving Show
instance FromJSON  SchemaDomain where
    parseJSON = A.withObject "SchemaDomain" $ \v ->
         SchemaDomain <$> v .:  "name"
            <*> v  .:  "version"


instance ToJSON SchemaDomain  where
    toJSON v = A.object
        [ "name" .= schemaDomainName v
        , "version" .= schemaDomainVersion v
        ]


data SchemaGetDomains = SchemaGetDomains {
    schemaGetDomainsDomains :: [SchemaDomain]
} deriving Show
instance FromJSON  SchemaGetDomains where
    parseJSON = A.withObject "SchemaGetDomains" $ \v ->
         SchemaGetDomains <$> v .:  "domains"



schemaGetDomains :: Session -> IO (Either Error SchemaGetDomains)
schemaGetDomains session  = sendReceiveCommandResult (conn session) ("Schema","getDomains") ([] ++ (catMaybes []))




