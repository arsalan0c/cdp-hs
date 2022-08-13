{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import Data.List

import qualified Protocol as P
import qualified Generate as Gen

doChromeDevtoolsProtocol :: IO String
doChromeDevtoolsProtocol = do 
    browserProtocolDomains <- P.topLevelDomains <$> P.parse "protocol/browser_protocol.json"
    jsProtocolDomains      <- P.topLevelDomains <$> P.parse "protocol/js_protocol.json"
    pure $ Gen.generate $ browserProtocolDomains ++ jsProtocolDomains
