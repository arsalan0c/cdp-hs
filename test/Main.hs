{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import ChromeDevtoolsProtocol

prop_test :: Property
prop_test = property $ do
    pure ()



main :: IO ()
main = defaultMain [checkParallel $$(discover)]
