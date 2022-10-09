{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CDP
    ( ClientApp
    , Handle
    , runClient
    , subscribe
    , unsubscribe

    , Command (..)
    , sendReceiveCommand
    , sendReceiveCommandResult

    , module CDP.Domains
    ) where

import Data.Proxy (Proxy)

import CDP.Domains
import CDP.Internal.Runtime
