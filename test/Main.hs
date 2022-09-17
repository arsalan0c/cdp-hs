{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main

import qualified CDP.Definition as D
import qualified CDP.Gen.Program as GP

prop_test :: Property
prop_test = property $ do
    pure ()

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
