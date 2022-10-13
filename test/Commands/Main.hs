{-# LANGUAGE TemplateHaskell #-}

module Commands.Main (main) where

import Hedgehog
import Hedgehog.Main
import Control.Monad
import Data.Maybe
import Data.Default

import qualified CDP as CDP

prop_browser_get_version :: Property
prop_browser_get_version = property $ void . 
    evalIO $ CDP.runClient def $ flip CDP.sendCommandWait CDP.PBrowserGetVersion

prop_dom_get_document :: Property
prop_dom_get_document = property $ void . 
    evalIO $ CDP.runClient def $ \handle ->
        CDP.sendCommandWait handle $ CDP.PDOMGetDocument Nothing Nothing

prop_emulation_can_emulate :: Property
prop_emulation_can_emulate = property $ void . 
        evalIO $ CDP.runClient def $ flip CDP.sendCommandWait CDP.PEmulationCanEmulate

prop_emulation_set_geolocationOverride :: Property
prop_emulation_set_geolocationOverride = property $
    evalIO $ CDP.runClient def $ \handle -> do
        CDP.sendCommandWait handle $
            CDP.PEmulationSetGeolocationOverride (Just 90) (Just 90) Nothing

-- prop_runtime_compileScript :: Property
-- prop_runtime_compileScript = property $ do
--     (res1, resE2) <- evalIO $ CDP.runClient def $ \handle -> do
--         CDP.runtimeEnable handle

--         -- compile
--         (Right res1) <- CDP.runtimeCompileScript handle $ 
--             CDP.PRuntimeCompileScript 
--                 "function fact(x) { return x < 0 ? 0 : x == 0 || x == 1 ? 1 : x * fact(x - 1) }" 
--                 "file:///Users/arsalanc-v2/Desktop/Dev/GSOC22/project/cdp-hs/app/CDP/fact.js" False Nothing
        
--         threadDelay 500000

--         -- run
--         res2 <- do
--             let (Just sid) = CDP.runtimeCompileScriptScriptId res1
--             CDP.runtimeRunScript handle $
--                 CDP.PRuntimeRunScript sid Nothing Nothing Nothing Nothing Nothing Nothing (Just True)

--         pure (res1, res2)
    
--     res2 <- evalEither resE2
--     evalIO $ print res2
--     CDP.runtimeCompileScriptExceptionDetails res1 === Nothing
--     CDP.runtimeRunScriptExceptionDetails     res2 === Nothing

--     val <- evalMaybe . CDP.runtimeRemoteObjectValue . CDP.runtimeRunScriptResult $ res2
--     val === 120

prop_network_cookies :: Property
prop_network_cookies = property $ do
    let name   = "key"
        value  = "value"
        domain = "localhost"

    (clear, set, cookies, clear2) <- evalIO $ CDP.runClient def $ \handle -> do
        clear   <- CDP.sendCommandWait handle CDP.PNetworkClearBrowserCookies
        set     <- CDP.sendCommandWait handle $
            CDP.PNetworkSetCookie name value Nothing (Just domain) Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing
        cookies <- CDP.sendCommandWait handle CDP.PNetworkGetAllCookies

        clear2  <- CDP.sendCommandWait handle CDP.PNetworkClearBrowserCookies
        pure (clear, set, cookies, clear2)

    let cks = CDP.networkGetAllCookiesCookies cookies
    length cks === 1

    let cookie = head cks
    CDP.networkCookieName   cookie === name
    CDP.networkCookieValue  cookie === value
    CDP.networkCookieDomain cookie === domain

prop_performance_get_metrics :: Property
prop_performance_get_metrics = property $ void . 
    evalIO $ CDP.runClient def $ flip CDP.sendCommandWait CDP.PPerformanceGetMetrics

prop_targets :: Property
prop_targets = property $ void . 
    evalIO $ CDP.runClient def $ \handle -> do
        CDP.sendCommandWait handle $
            CDP.PTargetCreateTarget "http://haskell.foundation" Nothing Nothing Nothing Nothing Nothing Nothing
 
main :: IO ()
main = defaultMain [checkSequential $$(discoverPrefix "prop")]
