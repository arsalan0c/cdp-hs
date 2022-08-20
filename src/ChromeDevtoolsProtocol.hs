{-# LANGUAGE OverloadedStrings   #-}

module ChromeDevtoolsProtocol where

import Data.List

import qualified Protocol as P
import qualified Generate as Gen
import qualified Data.Text as T

doChromeDevtoolsProtocol :: IO ()
doChromeDevtoolsProtocol = do 
    browserProtocolDomains <- P.topLevelDomains <$> P.parse "protocol/browser_protocol.json"
    jsProtocolDomains      <- P.topLevelDomains <$> P.parse "protocol/js_protocol.json"

    imports <- readFile "src/imports.txt"
    prelude <- readFile "src/prelude.txt"
    utils   <- readFile "src/utils.txt"
    let extensions = "{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, TupleSections  #-}"
    let (main, domains) = Gen.generate extensions imports prelude "import Utils" $ browserProtocolDomains ++ jsProtocolDomains

    writeFile "app/CDP/src/Utils.hs" $ 
        unlines 
            [ extensions
            , "module Utils where"
            , imports
            , utils
            ]
    writeFile "app/CDP/src/CDP.hs"   main
    _ <- mapM (\(d,prog) -> writeFile (mconcat ["app/CDP/src/Domains/", T.unpack d, ".hs"]) prog) domains
    pure ()



