{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad

import ChromeDevtoolsProtocol
import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting"
    CDP.runClient Nothing $ \session -> do
        CDP.eventSubscribe (CDP.EventNamePageWindowOpen) (putStrLn . CDP.pageWindowOpenUrl . f) session
        
        forever $ do
            threadDelay 1000
  where
    f (CDP.EventReturnPageWindowOpen v) = v

     