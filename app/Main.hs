{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import qualified System.FilePath as FP

import ChromeDevtoolsProtocol

main :: IO ()
main = do
    (main, domains) <- doChromeDevtoolsProtocol
    let basePath = ["app", "CDP", "src"]
    writeFile (FP.joinPath $ basePath ++ [FP.addExtension "CDP" "hs"]) main

    let domainsPath = basePath ++ ["Domains"]
    void $ mapM (\(name, code) -> writeFile (FP.joinPath . (domainsPath ++) . pure . flip FP.addExtension "hs" $ name) code) . 
        Map.toList $ 
        domains




     