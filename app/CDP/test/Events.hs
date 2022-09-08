{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Events (main) where

import Hedgehog
import Hedgehog.Main
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Default
import qualified Data.Map as Map

import qualified CDP as CDP
import qualified CDPPrelude as CDPPrelude

prop_page_frame :: Property
prop_page_frame = property $ do
    let config = def{CDPPrelude.doLogResponses = True}
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
        threadDelay 500000

        responses <- readMVar . CDPPrelude.responseBuffer . CDP.unhandle $ handle
        pure (enabled, nav, length responses)

    enabled === Nothing
    evalEither nav
    -- check at least 1 event was received, 2 responses are for the commands
    diff numResponses (>) 2 

main :: IO Bool
main = checkSequential . Group "events" $ [("page_frame", prop_page_frame)]