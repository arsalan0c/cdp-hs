{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CDP
    ( ClientApp
    , runClient
    , subscribe
    , unsubscribe

    , module CDP.Handle
    , module CDP.Domains
    ) where

import Data.Proxy (Proxy)

import CDP.Domains
import CDP.Handle
import CDP.Internal.Runtime

type ClientApp b = Handle -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config app

subscribe   :: Event a => Handle -> (a -> IO ()) -> IO ()
subscribe handle eventHandler = subscribe' handle eventHandler

unsubscribe :: Event a => Handle -> Proxy a -> IO ()
unsubscribe handle p = unsubscribe' handle p
