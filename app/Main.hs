{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified System.FilePath as FP
import qualified System.Directory as Dir

import qualified CDP.Definition as D
import qualified CDP.Gen.Program as GP

main :: IO ()
main = do
    domains <- fmap concat . mapM (fmap D.topLevelDomains . D.parse) $ 
        [ browserDefinitionPath
        , jsDefinitionPath 
        ] 

    domainExtensions <- fmap T.pack . readFile . FP.joinPath $ pathToPrelude ++ pathToDomains ++ ["DomainExtensions.txt"]
    domainImports    <- fmap T.pack . readFile . FP.joinPath $ pathToPrelude ++ pathToDomains ++ ["DomainImports.txt"]   
    let program = GP.genProgram domainExtensions domainImports $ domains

    Dir.removePathForcibly domainDir
    Dir.createDirectory domainDir
    mapM (\(dn,d) -> writeFile (domainPath . T.unpack . GP.unComponentName $ dn) (T.unpack d)) $ Map.toList . GP.pComponents $ program
    cdpExtensions <- fmap T.pack . readFile . FP.joinPath $ pathToPrelude ++ ["CDPExtensions.txt"]
    cdpImports    <- fmap T.pack . readFile . FP.joinPath $ pathToPrelude ++ ["CDPImports.txt"]
    
    libraryMain <- readFile cdpPreludePath
    let protocol = T.unpack . GP.genProtocolModule cdpExtensions cdpImports (Map.keys . GP.pComponents $ program) $
          T.intercalate "\n\n" 
            [ GP.pComponentImports program
            , T.pack libraryMain
            , GP.pEvents program
            ]
    
    writeFile cdpPath protocol
  where
    cdpPath        = FP.joinPath $ pathToGen ++ pathToCDP
    domainPath dn  = domainDir FP.</> FP.addExtension dn "hs"
    domainDir      = FP.joinPath $ pathToGen ++ pathToDomains   

    cdpPreludePath = FP.joinPath $ pathToPrelude ++ pathToCDP
    pathToCDP      = [FP.addExtension "CDP" "hs"]
    pathToDomains  = [libName, "Domains"]
    libName = "CDP"

    pathToGen      = [libFolder, "src"]
    pathToPrelude  = [libFolder, "prelude"]
    libFolder = "cdp"

    jsDefinitionPath      = FP.joinPath [definitionFolder, "js_protocol.json"]
    browserDefinitionPath = FP.joinPath [definitionFolder, "browser_protocol.json"]
    definitionFolder = "protocol"
