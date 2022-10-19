{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Pretty as R

import qualified CDP as CDP

main :: IO ()
main = do
    CDP.endpoint def CDP.EPFrontend >>= 
            print . R.renderHtml . H.toHtml . BS.toString
