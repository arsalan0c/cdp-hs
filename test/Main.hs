{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

import Test.Hspec
import Control.Monad
import Data.Default
import Control.Concurrent

import qualified CDP as CDP

main :: IO ()
main = hspec $ do
    describe "Endpoints responses of the expected type are received" $ do
        it "sends requests to all endpoints" $ do
            let cfg = def
            targetId <- fmap CDP.tiId . CDP.endpoint cfg $ CDP.EPOpenNewTab "https://haskell.foundation"

            void $ mapM (CDP.fromSomeEndpoint $ void . CDP.endpoint cfg)
                [ CDP.SomeEndpoint CDP.EPBrowserVersion
                , CDP.SomeEndpoint CDP.EPAllTargets
                , CDP.SomeEndpoint CDP.EPCurrentProtocol
                , CDP.SomeEndpoint CDP.EPFrontend
                , CDP.SomeEndpoint $ CDP.EPActivateTarget targetId
                , CDP.SomeEndpoint $ CDP.EPCloseTarget targetId
                ]

    describe "Command responses of the expected type are received" $ do
        it "sends commands: w/o params w/o results" $ do
            CDP.runClient def $ \handle -> do
                CDP.sendCommandWait handle CDP.PBrowserCrashGpuProcess

        it "sends commands: w/o params w/ results" $ do
            void $ CDP.runClient def $ \handle -> do
                void $ CDP.sendCommandWait handle CDP.PBrowserGetVersion
                CDP.sendCommandWait handle CDP.PEmulationCanEmulate

        it "sends commands: w/ params w/o results" $ do
            CDP.runClient def $ \handle -> 
                CDP.sendCommandWait handle $
                    CDP.PEmulationSetGeolocationOverride (Just 90) (Just 90) Nothing
        
        it "sends commands: w/ params w/ results" $ do
            void $ CDP.runClient def $ \handle ->
                CDP.sendCommandWait handle $ CDP.PDOMGetDocument Nothing Nothing

    describe "Commands have the expected behaviour" $ do
        it "sets a cookie" $ do
            let name   = "key"
                value  = "value"
                domain = "localhost"

            cookies <- CDP.runClient def $ \handle -> do
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
            CDP.runClient def $ \handle -> do
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
