{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import Data.List
import qualified Data.Map as Map

import qualified Protocol as P
import qualified Generate as Gen

doChromeDevtoolsProtocol :: IO (String, Map.Map String String)
doChromeDevtoolsProtocol = do 
    browserProtocolDomains <- P.topLevelDomains <$> P.parse "protocol/browser_protocol.json"
    jsProtocolDomains      <- P.topLevelDomains <$> P.parse "protocol/js_protocol.json"
    
    extensions <- readFile "src/DomainExtensions.txt"
    imports    <- readFile "src/DomainImports.txt"
    let (importOfDomains, protocol, domains) = Gen.generate extensions imports $ browserProtocolDomains ++ jsProtocolDomains
    
    prelude <- intercalate "\n\n" <$> sequence
        [ readFile "src/PreludeExtensions.txt"
        , readFile "src/PreludeModuleDeclaration.txt"
        , readFile "src/PreludeImports.txt"
        , pure importOfDomains
        , readFile "src/PreludeCode.txt"
        ]
    pure $ (unlines [prelude, protocol], domains)
