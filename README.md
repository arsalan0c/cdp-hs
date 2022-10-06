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

Print a page:

```hs
module Main where

import Data.Default
import System.Process

import qualified CDP as CDP

main :: IO ()
main = do
    putStrLn "Starting CDP example"
    -- run the 'printPDF' program with default configuration
    CDP.runClient def printPDF

printPDF :: CDP.Handle CDP.Event -> IO ()
printPDF handle = do 
    -- the handle contains the websocket connection to the browser, amongst other state

    -- send the Page.printToPDF command to the protocol
    -- this command has all arguments as optional and we've specified none
    r <- CDP.pagePrintToPdf handle $ 
        CDP.PPagePrintToPdf Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    let path = "mypdf.pdf"     
    -- decode pdf data
    readProcess "base64" ["--decode", "-o", path] $ CDP.pagePrintToPdfData r
    -- open pdf file
    callCommand $ unwords ["open", path]
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
