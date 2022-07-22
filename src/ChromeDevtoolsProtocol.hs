{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import qualified Protocol as P
import qualified Generate as Gen

doChromeDevtoolsProtocol :: IO String
doChromeDevtoolsProtocol = do
    tl <- P.parse "protocol/browser_protocol.json"
    let domains = ["Browser", "Page"]
        stl = tl{P.topLevelDomains = filter ((`elem` domains) . P.domainsEltDomain) . P.topLevelDomains $ tl}
    pure $ Gen.generate stl
