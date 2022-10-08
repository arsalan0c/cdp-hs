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

type ClientApp b = Handle () -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config (app . Handle)

subscribe   :: forall a ev. Event a => Handle ev -> (a -> IO ()) -> IO ()
subscribe (Handle handle) eventHandler = subscribe' handle eventHandler

unsubscribe :: forall a ev. Event a => Handle ev -> Proxy a -> IO ()
unsubscribe (Handle handle) p = unsubscribe' handle p
