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
    (enabled, nav1, nav2, numResponses) <- evalIO $ CDP.runClient config $ \session -> do
        -- register handler
        CDP.subscribe session $ \frameNavigated -> do
            print $ CDP.pageFrameId . CDP.pageFrameNavigatedFrame $ frameNavigated

        -- enable events
        enabled  <- CDP.pageEnable session
        -- page navigations
        nav1     <- CDP.pageNavigate session $
            CDP.PPageNavigate "http://wikipedia.com" Nothing Nothing Nothing
        nav2     <- CDP.pageNavigate session $
            CDP.PPageNavigate "http://haskell.foundation" Nothing Nothing Nothing
        
        -- wait for events
        threadDelay 500000

        responses <- readMVar . CDPPrelude.responseBuffer . CDP.unSession $ session
        pure (enabled, nav1, nav2, length responses)

    enabled === Nothing
    evalEither nav1
    evalEither nav2
    diff numResponses (>) 3 -- (check at least 1 event was received, 3 responses are for the commands)

main :: IO Bool
main = checkSequential . Group "events" $ [("page_frame", prop_page_frame)]