module Main where

import CDP
import Data.Default (def)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()
main = CDP.runClient def $ \handle -> forever $ do
    sendCommand handle PTargetCreateTarget
        { pTargetCreateTargetUrl = "https://twitter.com/GabriellaG439"
        , pTargetCreateTargetWidth = Nothing
        , pTargetCreateTargetHeight = Nothing
        , pTargetCreateTargetBrowserContextId = Nothing
        , pTargetCreateTargetEnableBeginFrameControl = Nothing
        , pTargetCreateTargetNewWindow = Just False
        , pTargetCreateTargetBackground = Just False
        }

    threadDelay $ 10 * 1000 * 1000
