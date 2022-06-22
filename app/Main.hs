module Main where

import ChromeDevtoolsProtocol

main = do
  print $ "Hello from " ++ doChromeDevtoolsProtocol ++ "!"
  perform
