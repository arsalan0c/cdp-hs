{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import qualified Protocol as P
import qualified Generate as Gen

doChromeDevtoolsProtocol :: IO String
doChromeDevtoolsProtocol = do
    tl <- P.parse "protocol/browser_protocol.json"
    let browserDomain = tl{P.topLevelDomains = filter ((== "Browser") . P.domainsEltDomain) . P.topLevelDomains $ tl}
    pure $ Gen.generate browserDomain
