{-# LANGUAGE OverloadedStrings   #-}

module CDP.Gen.Library where

import Data.List
import qualified System.FilePath as FP

import qualified CDP.Definition as D
import qualified CDP.Gen.Program as GP

doGenerateLibrary :: IO String
doGenerateLibrary = do 
    browserProtocolDomains <- fmap D.topLevelDomains $ D.parse $ FP.joinPath ["protocol", "browser_protocol.json"]
    jsProtocolDomains      <- fmap D.topLevelDomains $ D.parse $ FP.joinPath ["protocol", "js_protocol.json"]
    
    pure $ GP.genProgram $ browserProtocolDomains ++ jsProtocolDomains