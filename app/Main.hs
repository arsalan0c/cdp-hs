{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map

import ChromeDevtoolsProtocol
import qualified CDP as CDP

main :: IO ()
main = do
  program <- doChromeDevtoolsProtocol
  -- putStrLn . show $ (A.decode "{\"id\":1,\"result\":{}}" :: Maybe (CDP.CommandResponseResult ())) 
  putStrLn program
  -- CDP.runClient Nothing $ \session -> do
  --     -- bv <- CDP.pageStopLoading session
  --     bv <- CDP.pagePrintToPDF session Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing -- :: IO (Either String CDP.PagePrintToPDF)
  --     print bv
  