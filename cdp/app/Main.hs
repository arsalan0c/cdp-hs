{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe
import Control.Concurrent
import Control.Monad
import Data.Proxy
import Data.List
import Data.Default
import System.Process

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"
    CDP.runClient def $ \handle -> do
        sessionId <- attachTarget handle
        subPageLoad handle $ Just sessionId

browser :: CDP.Handle CDP.Event -> IO ()
browser handle = print =<< CDP.browserGetVersion handle Nothing

-- Attaches to target, returning the session id
attachTarget :: CDP.Handle CDP.Event -> IO String
attachTarget handle = do
    -- get a target id
    ti <- head . CDP.targetGetTargetsTargetInfos <$> CDP.targetGetTargets handle Nothing
    let tid = CDP.targetTargetInfoTargetId ti
    -- get a session id by attaching to the target
    CDP.targetAttachToTargetSessionId <$> do
        CDP.targetAttachToTarget handle Nothing $ CDP.PTargetAttachToTarget tid (Just True)

-- | Subscribes to page load events, for a particular session
subPageLoad :: CDP.Handle CDP.Event -> Maybe String -> IO ()
subPageLoad handle sessionId = do
    -- register a handler for the page load event 
    CDP.subscribe handle sessionId (print . CDP.pageLoadEventFiredTimestamp)
    -- start receiving events
    CDP.pageEnable handle sessionId
    -- navigate to a page, triggering the page load event
    CDP.pageNavigate handle Nothing $
        CDP.PPageNavigate "http://haskell.foundation" Nothing Nothing Nothing Nothing
    -- stop the program from terminating, to keep receiving events
    forever $ do
        threadDelay 1000

printPDF :: CDP.Handle CDP.Event -> IO ()
printPDF handle = do
    -- send the Page.printToPDF command
    r <- CDP.pagePrintToPdf handle Nothing $ 
            CDP.PPagePrintToPdf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing $
                Just CDP.PPagePrintToPdfTransferModeReturnAsStream
    -- obtain stream handle from which to read pdf data
    let streamHandle = fromJust . CDP.pagePrintToPdfStream $ r
    -- read pdf data 24000 bytes at a time
    let params = CDP.PIoRead streamHandle Nothing $ Just 24000
    reads <- whileTrue (not . CDP.ioReadEof) $ CDP.ioRead handle Nothing params
    let dat = concatMap CDP.ioReadData $ reads
    -- decode pdf to a file
    let path   = "mypdf.pdf"
    readProcess "base64" ["--decode", "-o", path] dat
    callCommand $ unwords ["open", path]

whileTrue :: Monad m => (a -> Bool) -> m a -> m [a]
whileTrue f act = do
    a <- act
    if f a
        then pure . (a :) =<< whileTrue f act
        else pure [a]