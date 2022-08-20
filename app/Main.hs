{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad

import ChromeDevtoolsProtocol
-- import qualified CDP as CDP

main :: IO ()
main = do
    doChromeDevtoolsProtocol
    -- putStrLn "Starting CDP Gen"
  where
    a = 10

     