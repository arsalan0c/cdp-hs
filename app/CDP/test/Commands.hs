{-# LANGUAGE TemplateHaskell #-}

module Commands (main) where

import Hedgehog
import Hedgehog.Main
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Default

import qualified CDP as CDP

prop_browser_get_version :: Property
prop_browser_get_version = property $ void . 
    evalEitherM . 
        evalIO $ CDP.runClient def $ CDP.browserGetVersion

prop_dom_get_document :: Property
prop_dom_get_document = property $ void . 
    evalEitherM . 
        evalIO $ CDP.runClient def $ \session -> do
            res <- CDP.dOMGetDocument session $ 
                CDP.PDOMGetDocument Nothing Nothing
            pure res

prop_emulation_can_emulate :: Property
prop_emulation_can_emulate = property $ void . 
    evalEitherM . 
        evalIO $ CDP.runClient def $ CDP.emulationCanEmulate

prop_emulation_set_geolocationOverride :: Property
prop_emulation_set_geolocationOverride = property $ do
    res <- evalIO $ CDP.runClient def $ \session -> do
        CDP.emulationSetGeolocationOverride session $ 
            CDP.PEmulationSetGeolocationOverride (Just 90) (Just 90) Nothing
    res === Nothing

prop_runtime_compileScript :: Property
prop_runtime_compileScript = property $ do
    (res1, resE2) <- evalIO $ CDP.runClient def $ \session -> do
        CDP.runtimeEnable session

        -- compile
        (Right res1) <- CDP.runtimeCompileScript session $ 
            CDP.PRuntimeCompileScript 
                "function fact(x) { return x < 0 ? 0 : x == 0 || x == 1 ? 1 : x * fact(x - 1) }" 
                "" True Nothing
        
        -- run
        res2 <- do
            let (Just sid) = CDP.runtimeCompileScriptScriptId res1
            CDP.runtimeRunScript session $
                CDP.PRuntimeRunScript sid Nothing Nothing Nothing Nothing Nothing Nothing Nothing

        pure (res1, res2)

    res2 <- evalEither resE2
    CDP.runtimeCompileScriptExceptionDetails res1 === Nothing
    CDP.runtimeRunScriptExceptionDetails     res2 === Nothing

    val <- evalMaybe . CDP.runtimeRemoteObjectValue $ CDP.runtimeRunScriptResult res2
    val === 120



prop_network_cookies :: Property
prop_network_cookies = property $ do
    let name   = "key"
        value  = "value"
        domain = "localhost"

    (clear, set, cookies, clear2) <- evalIO $ CDP.runClient def $ \session -> do
        clear   <- CDP.networkClearBrowserCookies session
        set     <- CDP.networkSetCookie session $ 
            CDP.PNetworkSetCookie name value Nothing (Just domain) Nothing Nothing Nothing Nothing Nothing
        cookies <- CDP.networkGetAllCookies session

        clear2  <- CDP.networkClearBrowserCookies session
        pure (clear, set, cookies, clear2)

    clear === Nothing        
    set   === Nothing
    cks <- fmap CDP.networkGetAllCookiesCookies . evalEither $ cookies
    length cks === 1

    let cookie = head cks
    CDP.networkCookieName   cookie === name
    CDP.networkCookieValue  cookie === value
    CDP.networkCookieDomain cookie === domain

    clear2 === Nothing        

prop_performance_get_metrics :: Property
prop_performance_get_metrics = property $ void . 
    evalEitherM . 
        evalIO $ CDP.runClient def $ CDP.performanceGetMetrics

prop_targets :: Property
prop_targets = property $ void . 
    evalEitherM . 
        evalIO $ CDP.runClient def $ \session -> do
            CDP.targetCreateTarget session $
                CDP.PTargetCreateTarget "http://haskell.foundation" Nothing Nothing Nothing Nothing
 
main :: IO ()
main = defaultMain [checkSequential $$(discover)]
