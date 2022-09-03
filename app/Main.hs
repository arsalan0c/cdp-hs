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
    main <- doChromeDevtoolsProtocol
    let basePath = ["app", "CDP", "src"]
    writeFile (FP.joinPath $ basePath ++ [FP.addExtension "CDP" "hs"]) main
