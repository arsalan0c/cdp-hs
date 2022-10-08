{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import qualified System.IO as IO

import qualified CDP.Definition as D
import qualified CDP.Gen.Program as GP

main :: IO ()
main = do
    domains <- fmap concat . mapM (fmap D.topLevelDomains . D.parse) $ 
        [ browserDefinitionPath
        , jsDefinitionPath 
        ] 

    let program = GP.genProgram domains

    for_ (Map.toList . GP.pComponents $ program) $ \(dn,d) -> do
        let path = domainPath . T.unpack . GP.unComponentName $ dn
        IO.hPutStrLn IO.stderr $ "Writing domain to " ++ path ++ "..."
        T.writeFile path d

    let protocol = GP.genProtocolModule (Map.keys . GP.pComponents $ program) $
          GP.pComponentImports program

    IO.hPutStrLn IO.stderr $ "Writing protocol to " ++ protocolModulePath ++ "..."
    T.writeFile protocolModulePath protocol
  where
    domainPath dn         = domainDir FP.</> FP.addExtension dn "hs"
    domainDir             = "src/CDP/Domains"
    protocolModulePath    = "src/CDP/Domains.hs"
    jsDefinitionPath      = "protocol/js_protocol.json"
    browserDefinitionPath = "protocol/browser_protocol.json"
