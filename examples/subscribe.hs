module Main (main) where

import qualified CDP as CDP
import Data.Default (def)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Fix (mfix)

main :: IO ()
main = CDP.runClient def $ \handle -> do
    CDP.sendCommand handle CDP.pPageEnable
    sub1 <- mfix $ \sub -> CDP.subscribe handle $ \e -> do
        putStrLn $ "1: " ++ CDP.pageFrameUrl (CDP.pageFrameNavigatedFrame e)
        CDP.unsubscribe handle sub
    _ <- CDP.subscribe handle $ \e ->
        putStrLn $ "2: " ++ CDP.pageFrameUrl (CDP.pageFrameNavigatedFrame e)
    forever $ threadDelay 1000
