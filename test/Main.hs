{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Commands.Main as C
import qualified Events.Main as E

main :: IO ()
main = do
    C.main
    E.main
