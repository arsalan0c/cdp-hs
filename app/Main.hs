{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified System.FilePath as FP

import CDP.Gen.Library

main :: IO ()
main = do
    let preludePath  = ["cdp", "prelude"]
        genPath      = ["cdp", "src"]
        protocolPath = ["CDP", FP.addExtension "Protocol" "hs"]
        mainPath     = [FP.addExtension "CDP" "hs"]

    protocol        <- doGenerateLibrary
    protocolPrelude <- readFile $ FP.joinPath $ preludePath ++ protocolPath
    writeFile (FP.joinPath $ genPath ++ protocolPath) $ unlines [protocolPrelude, protocol]

    mainPrelude <- readFile $ FP.joinPath $ preludePath ++ mainPath 
    writeFile (FP.joinPath $ genPath ++ mainPath) mainPrelude
