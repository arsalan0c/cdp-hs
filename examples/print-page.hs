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
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"
    CDP.runClient def printPDF

targets :: CDP.Handle -> IO ()
targets handle = do
    ti <- head . CDP.targetGetTargetsTargetInfos <$> CDP.targetGetTargets handle
    let tid = CDP.targetTargetInfoTargetId ti
    r <- CDP.targetAttachToTarget handle $ CDP.PTargetAttachToTarget tid (Just True)
    print r
    browser handle 

browser :: CDP.Handle -> IO ()
browser handle = print =<< CDP.browserGetVersion handle

subUnsub :: CDP.Handle -> IO ()
subUnsub handle = do
    sub <- CDP.subscribe handle (print . CDP.pageWindowOpenUrl)
    CDP.pageEnable handle
    CDP.unsubscribe handle sub

    forever $ do
        threadDelay 1000

printPDF :: CDP.Handle -> IO ()
printPDF handle = do
    -- send the Page.printToPDF command
    r <- CDP.pagePrintToPDF handle $
            CDP.PPagePrintToPDF Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing $
                CDP.PPagePrintToPDFTransferModeReturnAsStream

    -- obtain stream handle from which to read pdf data
    let streamHandle = fromJust . CDP.pagePrintToPDFStream $ r

    -- read pdf data 24000 bytes at a time, this could be refactored to
    -- properly take the 'encoded' flag into account and produce a lazy
    -- bytestring
    let params = CDP.PIORead streamHandle Nothing $ Just 24000
    reads <- whileTrue (not . CDP.iOReadEof) $ CDP.iORead handle params
    let dat = map (Base64.decodeLenient . T.encodeUtf8 . T.pack . CDP.iOReadData) reads
    B.writeFile "mypdfs.pdf" $ B.concat dat

whileTrue :: Monad m => (a -> Bool) -> m a -> m [a]
whileTrue f act = do
    a <- act
    if f a
        then pure . (a :) =<< whileTrue f act
        else pure [a]
