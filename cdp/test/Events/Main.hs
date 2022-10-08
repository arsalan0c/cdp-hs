{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Main (main) where

import Hedgehog
import Hedgehog.Main
import Control.Monad
import Control.Concurrent
import Data.Default

import CDP as CDP
import CDP.Handle
import CDP.Internal.Runtime

-- Takes too long
-- prop_page_frame :: Property
-- prop_page_frame = property $ do
--     let config = def{CDP.doLogResponses = True}
--     (enabled, nav, numResponses) <- evalIO $ CDP.runClient config $ \handle@(Handle handle') -> do
--         -- register handler
--         CDP.subscribe handle $ \frameNavigated -> do
--             print $ CDP.pageFrameId . CDP.pageFrameNavigatedFrame $ frameNavigated
--         -- enable events
--         enabled  <- CDP.pageEnable handle Nothing
--         -- navigate to page
--         nav     <- CDP.pageNavigate handle Nothing $
--             CDP.PPageNavigate "http://wikipedia.com" Nothing Nothing Nothing Nothing
--         -- wait for events
--         threadDelay 2000000
--         -- check the response buffer
--         responses <- readMVar . responseBuffer $ handle'
--         pure (enabled, nav, length responses)

--     enabled === Nothing
--     void $ evalEither nav
--     -- check at least 1 event was received, 2 responses are for the commands
--     diff numResponses (>) 2

main :: IO ()
main = defaultMain [checkSequential $$(discover)]