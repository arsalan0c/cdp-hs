{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE Strict   #-}

module Main where

import qualified Data.Text as T
import qualified System.FilePath as FP

import qualified CDP.Definition as D
import qualified CDP.Gen.Program as GP

main :: IO ()
main = do
    let preludePath  = ["cdp", "prelude"]
        genPath      = ["cdp", "src"]
        protocolPath = ["CDP", FP.addExtension "Protocol" "hs"]
        mainPath     = [FP.addExtension "CDP" "hs"]
    
    browserProtocolDomains <- fmap D.topLevelDomains $ D.parse $ FP.joinPath ["protocol", "browser_protocol.json"]
    jsProtocolDomains      <- fmap D.topLevelDomains $ D.parse $ FP.joinPath ["protocol", "js_protocol.json"]    
    let program = T.unpack . GP.genProgram $ browserProtocolDomains ++ jsProtocolDomains

    protocolPrelude <- readFile $ FP.joinPath $ preludePath ++ protocolPath
    writeFile (FP.joinPath $ genPath ++ protocolPath) 
        (unlines [protocolPrelude, program])

    libraryMain <- readFile $ FP.joinPath $ preludePath ++ mainPath 
    writeFile (FP.joinPath $ genPath ++ mainPath) libraryMain
