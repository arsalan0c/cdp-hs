{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad
import Data.Proxy
import Data.Default
import System.Process

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"
    CDP.runClient def browser

browser :: CDP.Handle CDP.Event -> IO ()
browser handle = print =<< CDP.browserGetVersion handle

subUnsub :: CDP.Handle CDP.Event -> IO ()
subUnsub handle = do
    CDP.subscribe handle (print . CDP.pageWindowOpenUrl)
    CDP.pageEnable handle
    CDP.unsubscribe handle (Proxy :: Proxy CDP.PageWindowOpen)

    forever $ do
        threadDelay 1000

printPDF :: CDP.Handle CDP.Event -> IO ()
printPDF handle = do
    r <- CDP.pagePrintToPdf handle $ 
        CDP.PPagePrintToPdf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing $
            CDP.PPagePrintToPdfTransferModeReturnAsBase64

    let dat  = CDP.pagePrintToPdfData r
        path = "mypdf.pdf" 
     
    readProcess "base64" ["--decode", "-o", path] dat
    callCommand $ unwords ["open", path]