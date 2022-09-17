{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Main (main) where

import Hedgehog
import Hedgehog.Main
import Control.Concurrent
import Control.Monad
import Data.Default

import qualified CDP as CDP
import qualified CDP.Runtime as R

prop_page_frame :: Property
prop_page_frame = property $ do
    let config = def{CDP.doLogResponses = True}
    (enabled, nav, numResponses) <- evalIO $ CDP.runClient config $ \handle -> do
        -- register handler
        CDP.subscribe handle $ \frameNavigated -> do
            print $ CDP.pageFrameId . CDP.pageFrameNavigatedFrame $ frameNavigated
        -- enable events
        enabled  <- CDP.pageEnable handle
        -- navigate to page
        nav     <- CDP.pageNavigate handle $
            CDP.PPageNavigate "http://wikipedia.com" Nothing Nothing Nothing
        -- wait for events
        threadDelay 300000
        -- check the response buffer
        responses <- readMVar . R.responseBuffer $ handle
        pure (enabled, nav, length responses)

    enabled === Nothing
    void $ evalEither nav
    -- check at least 1 event was received, 2 responses are for the commands
    diff numResponses (>) 2 

main :: IO ()
main = defaultMain [checkSequential $$(discover)]