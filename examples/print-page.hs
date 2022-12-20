{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.Maybe
import Data.Default
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified CDP as CDP

main :: IO ()
main = do
    let cfg = def
    CDP.runClient cfg printPDF

printPDF :: CDP.Handle -> IO ()
printPDF handle = do
    -- send the Page.printToPDF command
    r <- CDP.sendCommandWait handle $ CDP.pPagePrintToPDF
        { CDP.pPagePrintToPDFTransferMode = Just CDP.PPagePrintToPDFTransferModeReturnAsStream
        }

    -- obtain stream handle from which to read pdf data
    let streamHandle = fromJust . CDP.pagePrintToPDFStream $ r

    -- read pdf data 24000 bytes at a time
    let params = CDP.PIORead streamHandle Nothing $ Just 24000
    reads <- whileTrue (not . CDP.iOReadEof) $ CDP.sendCommandWait handle params
    let dat = map decode reads
    BL.writeFile "mypdf.pdf" $ BL.concat dat

decode :: CDP.IORead -> BL.ByteString
decode ior = if (CDP.iOReadBase64Encoded ior == Just True)
    then Base64.decodeLenient lbs
    else lbs
  where
    lbs = TL.encodeUtf8 . TL.fromStrict . CDP.iOReadData $ ior

whileTrue :: Monad m => (a -> Bool) -> m a -> m [a]
whileTrue f act = do
    a <- act
    if f a
        then pure . (a :) =<< whileTrue f act
        else pure [a]
