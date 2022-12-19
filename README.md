[![build](https://github.com/arsalan0c/cdp-hs/actions/workflows/build.yaml/badge.svg)](https://github.com/arsalan0c/cdp-hs/actions/workflows/build.yaml)
# cdp-hs

A Haskell library for the [Chrome Devtools Protocol (CDP)](https://chromedevtools.github.io/devtools-protocol/), generated from the protocol's definition files.

## Quick start

To get started:

1. Run Chromium with debugging port enabled: `chromium --headless --remote-debugging-port=9222 http://wikipedia.com`
2. Clone the repo with submodules `git clone --recurse-submodules https://github.com/arsalan0c/cdp-hs.git`
3. Switch directories: `cd cdp-hs`
4. Drop into nix environment: `nix-shell --pure`
5. Generate cabal file: `hpack`
6. Run the example program to print a page as pdf: `cabal run cdp-example-print-page`

## Example usage

Print a page to PDF, with Base64 encoded data being read in chunks:

```hs
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.Maybe
import Data.Default
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified CDP as CDP

main :: IO ()
main = do
    let cfg = def
    -- connect to a new page
    id <- CDP.tiId <$> CDP.connectToTab cfg "https://haskell.foundation" 
    CDP.runClient cfg (printPDF id)

printPDF :: T.Text -> CDP.Handle -> IO ()
printPDF targetId handle = do
    -- send the Page.printToPDF command
    r <- CDP.sendCommandForSessionWait handle targetId $ CDP.pPagePrintToPDF
        { CDP.pPagePrintToPDFTransferMode = Just CDP.PPagePrintToPDFTransferModeReturnAsStream
        }

    -- obtain stream handle from which to read pdf data
    let streamHandle = fromJust . CDP.pagePrintToPDFStream $ r

    -- read pdf data 24000 bytes at a time
    let params = CDP.PIORead streamHandle Nothing $ Just 24000
    reads <- whileTrue (not . CDP.iOReadEof) $ CDP.sendCommandWait handle params
    let dat = map decode reads
    BL.writeFile "mypdf.pdf" $ BL.concat dat

decode :: CDP.IORead -> BL.ByteString
decode ior = if (CDP.iOReadBase64Encoded ior == Just True)
    then Base64.decodeLenient lbs
    else lbs
  where
    lbs = TL.encodeUtf8 . TL.fromStrict . CDP.iOReadData $ ior

whileTrue :: Monad m => (a -> Bool) -> m a -> m [a]
whileTrue f act = do
    a <- act
    if f a
        then pure . (a :) =<< whileTrue f act
        else pure [a]
```

## Generating the CDP library

1. Build the generator library: `nix-build --attr exe`
2. Run the generator: `result/bin/gen-exe`

`cdp`, the library folder, will contain the newly generated code.

## Current state

[Project board](https://github.com/users/arsalan0c/projects/1)

Commands and events for all non-deprecated domains are supported.
The following session functionalities are supported:
- creating a session: obtain a session id by using the `pTargetAttachToTarget` function to send a `Target.attachToTarget` command, passing `True` for the flatten argument
- send a command for a particular session: use the `sendCommandForSession` function with a session id
- subscribe to events for a particular session: 
  1. register a handler with a session id 
  2. send the command to enable events for the domain, with the same session id

## Contributing

PRs are welcome! If you would like to discuss changes or have any feedback, feel free to open an [issue](https://github.com/arsalan0c/cdp-hs/issues).


## Acknowledgements

This began as a [Summer of Haskell](https://summer.haskell.org) / [GSoC](https://summerofcode.withgoogle.com) project. Albert Krewinkel ([@tarleb](https://github.com/tarleb)), Jasper Van der Jeugt ([@jaspervdj](https://github.com/jaspervdj)) and Romain Lesur ([@RLesur](https://github.com/rlesur)) provided valuable feedback and support which along with raising the library's quality, has made this all the more enjoyable to work on.

## References

- https://jaspervdj.be/posts/2013-09-01-controlling-chromium-in-haskell.html
- https://www.jsonrpc.org/specification
