module Main (main) where

import qualified CDP as CDP
import Data.Default (def)

main :: IO ()
main = CDP.runClient def $ \handle -> do
    CDP.subscribe handle (\pageNavigated -> putStrLn $
        "Navigated to: " ++ CDP.pageFrameUrl (CDP.pageFrameNavigatedFrame pageNavigated))
    CDP.pageEnable handle
