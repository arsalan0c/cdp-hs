[![build](https://github.com/arsalan0c/cdp-hs/actions/workflows/build.yaml/badge.svg)](https://github.com/arsalan0c/cdp-hs/actions/workflows/build.yaml)
# cdp-hs

A Haskell library for the [Chrome Devtools Protocol (CDP)](https://chromedevtools.github.io/devtools-protocol/), generated from the protocol's definition files.


## Quick start

`cdp` contains the generated library.

To get started:

1. Clone the repo with submodules `git clone --recurse-submodules https://github.com/arsalan0c/cdp-hs.git`
2. Switch directories to the CDP library: `cd cdp`
3. Drop into nix environment: `nix-shell --pure`
4. Generate cabal file: `hpack`
5. Run Chromium with debugging port enabled: `chromium --headless --remote-debugging-port=9222 http://wikipedia.com`
6. Run the example program to print the [browser's version info](https://chromedevtools.github.io/devtools-protocol/tot/Browser/#method-getVersion) : `cabal run cdp-exe`

## Example usage

Print a page to PDF, with Base64 encoded data being read in chunks:

```hs
module Main where

import Data.Default
import Data.Maybe
import System.Process

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"
    -- run the 'printPDF' program with default configuration
    CDP.runClient def printPDF

printPDF :: CDP.Handle CDP.Event -> IO ()
printPDF handle = do
    -- send the Page.printToPDF command
    r <- CDP.pagePrintToPdf handle $ 
            CDP.PPagePrintToPdf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing $
                CDP.PPagePrintToPdfTransferModeReturnAsStream
    
    -- obtain stream handle from which to read pdf data
    let streamHandle = fromJust . CDP.pagePrintToPdfStream $ r

    -- read pdf data 24000 bytes at a time
    let params = CDP.PIoRead streamHandle Nothing $ Just 24000
    reads <- whileTrue (not . CDP.ioReadEof) $ CDP.ioRead handle params
    let dat = concatMap CDP.ioReadData $ reads

    -- decode pdf to a file
    let path   = "mypdfs.pdf"
    readProcess "base64" ["--decode", "-o", path] dat
    callCommand $ unwords ["open", path]

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

Upcoming features:
- sessions

## Acknowledgements

This began as a [Summer of Haskell](https://summer.haskell.org) / [GSoC](https://summerofcode.withgoogle.com) project. Albert Krewinkel ([@tarleb](https://github.com/tarleb)), Jasper Van der Jeugt ([@jaspervdj](https://github.com/jaspervdj)) and Romain Lesur ([@RLesur](https://github.com/rlesur)) provided valuable feedback and support which along with raising the library's quality, has made this all the more enjoyable to work on.

## References

- https://jaspervdj.be/posts/2013-09-01-controlling-chromium-in-haskell.html
- https://www.jsonrpc.org/specification
