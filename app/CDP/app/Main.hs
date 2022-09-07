{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad
import Data.Proxy
import Data.Default

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"

    CDP.runClient def $ \session -> do
        print =<< CDP.browserGetVersion session

        CDP.subscribe session (print . CDP.pageWindowOpenUrl)
        CDP.pageEnable session
        CDP.unsubscribe session (Proxy :: Proxy CDP.PageWindowOpen)

        forever $ do
            threadDelay 1000