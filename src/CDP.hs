{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CDP
    ( ClientApp
    , Handle
    , Config(..)
    , runClient

    , Subscription
    , subscribe
    , unsubscribe

    , Command (..)
    , Promise (..)
    , readPromise
    , sendCommand
    , sendCommandWait

    , module CDP.Domains
    ) where

import Data.Proxy (Proxy)

import CDP.Domains
import CDP.Runtime
