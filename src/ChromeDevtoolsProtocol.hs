{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import Data.List

import qualified Protocol as P
import qualified Generate as Gen

doChromeDevtoolsProtocol :: IO String
doChromeDevtoolsProtocol = do 
    generatedBrowserProtocol <- Gen.generate <$> P.parse "protocol/browser_protocol.json"
    generatedJSProtocol <- Gen.generate <$> P.parse "protocol/js_protocol.json"
    pure . intercalate "\n" $ [generatedBrowserProtocol, generatedJSProtocol]
