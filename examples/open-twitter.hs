module Main where

import CDP
import Data.Default (def)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = CDP.runClient def $ \handle -> forever $ do
    sendCommand handle $ pTargetCreateTarget "https://twitter.com/GabriellaG439"
    threadDelay $ 10 * 1000 * 1000
