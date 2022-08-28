{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad
import Data.Proxy

import ChromeDevtoolsProtocol
import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP"
    doChromeDevtoolsProtocol >>= appendFile "app/CDP.hs"
    
    CDP.runClient Nothing $ \session -> do
        print =<< CDP.browserGetVersion session

        CDP.subscribe session (print . CDP.pageWindowOpenUrl)
        CDP.pageEnable session
        -- CDP.unsubscribe session (Proxy :: Proxy CDP.PageWindowOpen)

        forever $ do
            threadDelay 1000

     