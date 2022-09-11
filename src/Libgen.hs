{-# LANGUAGE OverloadedStrings   #-}

module Libgen where

import Data.List

import qualified Protocol as P
import qualified Codegen as Gen

doGenerateLibrary :: IO String
doGenerateLibrary = do 
    browserProtocolDomains <- P.topLevelDomains <$> P.parse "protocol/browser_protocol.json"
    jsProtocolDomains      <- P.topLevelDomains <$> P.parse "protocol/js_protocol.json"
    
    prelude <- readFile "CDP/prelude/CDP.txt"
    let protocol = Gen.generate $ browserProtocolDomains ++ jsProtocolDomains
    pure $ unlines [prelude, protocol]
