{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

import Test.Hspec
import Control.Monad
import Data.Default
import Control.Concurrent

import qualified CDP as CDP

connectToPage :: CDP.Config -> IO CDP.TargetInfo
connectToPage cfg = do
    targetInfo <- CDP.endpoint cfg $ CDP.EPOpenNewTab "https://haskell.foundation"
    CDP.endpoint cfg $ CDP.EPActivateTarget targetId
    pure targetInfo

main :: IO ()
main = hspec $ do
    
    describe "Endpoints responses of the expected type are received" $ do
        it "sends requests to all endpoints" $ do
            let cfg = def
            targetId <- fmap CDP.tiId $ connectToPage cfg
            void $ mapM (CDP.fromSomeEndpoint $ void . CDP.endpoint cfg)
                [ CDP.SomeEndpoint CDP.EPBrowserVersion
                , CDP.SomeEndpoint CDP.EPAllTargets
                , CDP.SomeEndpoint CDP.EPCurrentProtocol
                , CDP.SomeEndpoint CDP.EPFrontend
                , CDP.SomeEndpoint $ CDP.EPCloseTarget targetId
                ]

    (host,port,path) <- fmap CDP.parseUri $ runIO $ connectToPage def
    let cfg = def{CDP.hostPort = (host,port), CDP.path = Just path}
    describe "Command responses of the expected type are received" $ do
        it "sends commands: w/o params w/o results" $ do
            CDP.runClient cfg $ \handle -> do
                CDP.sendCommandForSessionWait handle targetId CDP.PBrowserCrashGpuProcess
        
        
        it "sends commands: w/o params w/ results" $ do
            void $ CDP.runClient cfg $ \handle -> do
                mapM (CDP.fromSomeCommand $ void . (CDP.sendCommandWait handle)) $
                    [ CDP.SomeCommand CDP.PBrowserGetVersion
                    , CDP.SomeCommand CDP.PEmulationCanEmulate
                    ]

        it "sends commands: w/ params w/o results" $ do
            CDP.runClient cfg $ \handle -> 
                CDP.sendCommandWait handle $
                    CDP.PEmulationSetGeolocationOverride (Just 90) (Just 90) Nothing
        
        it "sends commands: w/ params w/ results" $ do
            void $ CDP.runClient cfg $ \handle ->
                CDP.sendCommandWait handle $ CDP.PDOMGetDocument Nothing Nothing

    describe "Commands have the expected behaviour" $ do
        it "sets a cookie" $ do
            let name   = "key"
                value  = "value"
                domain = "localhost"

            cookies <- CDP.runClient cfg $ \handle -> do
                CDP.sendCommandWait handle CDP.PNetworkClearBrowserCookies
                CDP.sendCommandWait handle $
                    CDP.PNetworkSetCookie name value Nothing (Just domain) Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing
                CDP.sendCommandWait handle CDP.PNetworkGetAllCookies
    
            let cks = CDP.networkGetAllCookiesCookies cookies
            length cks `shouldBe` 1
        
            let cookie = head cks
            CDP.networkCookieName   cookie `shouldBe` name
            CDP.networkCookieValue  cookie `shouldBe` value
            CDP.networkCookieDomain cookie `shouldBe` domain

    describe "Expected events are triggered" $ do
        it "navigates to a page" $ do
            frameIdsM <- newMVar []
            CDP.runClient cfg $ \handle -> do
                -- register handler
                void $ CDP.subscribe handle $ \e -> modifyMVar_ frameIdsM $ 
                    \ids -> pure ((CDP.pageFrameId . CDP.pageFrameNavigatedFrame $ e) : ids)
                -- enable events
                CDP.sendCommandWait handle $ CDP.PPageEnable
                -- navigate to page
                void $ CDP.sendCommandWait handle $
                    CDP.PPageNavigate "http://wikipedia.com" Nothing Nothing Nothing Nothing
                -- wait for events
                threadDelay 5000000
            
            -- check at least 1 event was received
            ids <- readMVar frameIdsM
            length ids `shouldSatisfy` (> 0)                
