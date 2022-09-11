{- functions used by generated code -}

{-# LANGUAGE ScopedTypeVariables #-}

module Internal (module Internal) where

import Data.Char

uncapitalizeFirst :: String -> String
uncapitalizeFirst [] = []
uncapitalizeFirst (hd:tl) = toLower hd : tl