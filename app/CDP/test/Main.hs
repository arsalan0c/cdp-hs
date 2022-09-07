{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad

import qualified Commands as C
import qualified Events as E

main :: IO ()
main = do
    C.main
    void $ E.main
