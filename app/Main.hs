{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.List (intercalate)

import ChromeDevtoolsProtocol
import qualified CDP as CDP

main :: IO ()
main = do
  program <- doChromeDevtoolsProtocol
  let augProgram = intercalate "\n\n" $ [ "{-# LANGUAGE OverloadedStrings   #-}", "module CDP where", imports, pageInfo, program, main']--, browserVersion]
  putStrLn augProgram
  CDP.main
 
imports = "\
  \import           Control.Applicative  ((<$>))\n\
  \import           Control.Monad\n\
  \import           Control.Monad.Trans  (liftIO)\n\
  \import qualified Data.Map             as M\n\
  \import           Data.Maybe           (catMaybes, fromMaybe)\n\
  \import Data.String\n\
  \import qualified Data.Text as T\n\
  \import qualified Data.Text.IO         as TI\n\
  \import qualified Data.Vector          as V\n\ 
  \import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))\n\
  \import qualified Data.Aeson           as A\n\
  \import qualified Network.HTTP.Simple as Http\n\
  \import qualified Network.URI          as Uri\n\
  \import qualified Network.WebSockets as WS"

browserVersion = "\
  \data BrowserVersion = BrowserVersion {\n\
  \    protocolVersion :: String\n\
  \,   product :: String\n\
  \,   revision :: String\n\
  \,   userAgent :: String\n\
  \,   jsVersion :: String\n\
  \} deriving Show\n\

  \instance FromJSON BrowserVersion where\n\
  \    parseJSON = A.withObject \"BrowserVersion\" $ \\v ->\n\
  \        BrowserVersion <$> v .: \"protocolVersion\"\n\
  \            <*> v .: \"product\"\n\
  \            <*> v .: \"revision\"\n\
  \            <*> v .: \"userAgent\"\n\
  \            <*> v .: \"jsVersion\""

main' = "\
  \main :: IO ()\n\
  \main = do\n\
  \    pi <- getPageInfo \"http://127.0.0.1:9222/json\"\n\
  \    putStrLn $ show pi\n\
  \    let (host, port, path) = parseUri (debuggerUrl pi)\n\
  \    WS.runClient host port path $ \\conn -> do\n\
  \        bv <- browserGetVersion conn :: IO (Maybe (CommandResult BrowserGetVersion))\n\
  \        print bv"

pageInfo = "\
  \data PageInfo = PageInfo\n\
  \    { debuggerUrl :: String\n\
  \    } deriving Show\n\
  \instance FromJSON PageInfo where\n\
  \    parseJSON = A.withObject \"PageInfo\" $ \\v ->\n\
  \        PageInfo <$> v .: \"webSocketDebuggerUrl\"\n\
  
  \parseUri :: String -> (String, Int, String)\n\
  \parseUri uri = fromMaybe (error \"parseUri: Invalid URI\") $ do\n\
  \    u    <- Uri.parseURI uri\n\
  \    auth <- Uri.uriAuthority u\n\
  \    let port = case Uri.uriPort auth of\n\
  \            (':':str)   -> read str\n\
  \            _           -> 80\n\
  
  \    pure (Uri.uriRegName auth, port, Uri.uriPath u)\n\
  
  \getPageInfo :: Http.Request -> IO PageInfo\n\
  \getPageInfo request = do\n\
  \    response <- Http.httpLBS request\n\
  \    let body = Http.getResponseBody response\n\
  \    case A.decode body of\n\
  \        Just mpis -> pure $ head . catMaybes $ mpis\n\
  \        Nothing   -> error \"getPageInfo: Parse error\""