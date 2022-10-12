module Main (main) where

import qualified CDP as CDP
import Data.Default (def)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = CDP.runClient def $ \handle -> do
    CDP.sendCommand handle CDP.PPageEnable
    sub1 <- CDP.subscribe handle $ \e ->
        putStrLn $ "1: " ++ CDP.pageFrameUrl (CDP.pageFrameNavigatedFrame e)
    _ <- CDP.subscribe handle $ \e -> do
        putStrLn $ "2: " ++ CDP.pageFrameUrl (CDP.pageFrameNavigatedFrame e)
        CDP.unsubscribe handle sub1
    forever $ threadDelay 1000
