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

    CDP.runClient def $ \handle -> do
        print =<< CDP.browserGetVersion handle

        CDP.subscribe handle (print . CDP.pageWindowOpenUrl)
        CDP.pageEnable handle
        CDP.unsubscribe handle (Proxy :: Proxy CDP.PageWindowOpen)

        forever $ do
            threadDelay 1000