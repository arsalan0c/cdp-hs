{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified System.FilePath as FP

import Libgen

main :: IO ()
main = do
    main <- doGenerateLibrary
    let basePath = ["app", "CDP", "src"]
    writeFile (FP.joinPath $ basePath ++ [FP.addExtension "CDP" "hs"]) main
