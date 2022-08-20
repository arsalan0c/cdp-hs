{-# LANGUAGE OverloadedStrings   #-}

module Main where

import qualified Network.WebSockets as WS
import qualified Data.Aeson           as A
import Data.Maybe (fromMaybe)
import Data.Map
import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
    -- doChromeDevtoolsProtocol
    putStrLn "Starting CDP"
    -- CDP.runClient Nothing $ \session -> do
    --   -- print =<< CDP.pagePrintToPDF session Nothing (Just True) (Just True) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    --     CDP.eventSubscribe (CDP.EventNamePageWindowOpen) (putStrLn . CDP.pageWindowOpenUrl . f) session
        
    --     forever $ do
    --         threadDelay 1000
  where
    a = 10
    -- f (CDP.EventReturnPageWindowOpen v) = v

     