{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Main (main) where

import Hedgehog
import Hedgehog.Main
import Control.Monad
import Control.Concurrent
import Data.Default

import CDP as CDP
import CDP.Runtime

-- Takes too long
-- prop_page_frame :: Property
-- prop_page_frame = property $ do
--     let config = def{CDP.doLogResponses = True}
--     (_, _, numResponses) <- evalIO $ CDP.runClient config $ \handle@(Handle handle') -> do
--         -- register handler
--         CDP.subscribe handle $ \frameNavigated -> do
--             print $ CDP.pageFrameId . CDP.pageFrameNavigatedFrame $ frameNavigated
--         -- enable events
--         enabled  <- CDP.sendCommandWait handle CDP.PPageEnable
--         -- navigate to page
--         nav     <- CDP.sendCommandWait handle $
--             CDP.PPageNavigate "http://wikipedia.com" Nothing Nothing Nothing Nothing
--         -- wait for events
--         threadDelay 2000000
--         -- check the response buffer
--         responses <- readMVar . responseBuffer $ handle'
--         pure (enabled, nav, length responses)

--     -- check at least 1 event was received, 2 responses are for the commands
--     diff numResponses (>) 2

main :: IO ()
main = defaultMain [checkSequential $$(discover)]
