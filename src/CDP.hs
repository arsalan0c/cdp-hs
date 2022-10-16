{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CDP
    ( Error             (..)
    , ProtocolError     (..)
    , EPBrowserVersion  (..)
    , EPAllTargets      (..)
    , EPCurrentProtocol (..)
    , EPOpenNewTab      (..)
    , EPActivateTarget  (..)
    , EPCloseTarget     (..)
    , EPFrontend        (..)
    , Endpoint
    , EndpointResponse
    , IsEndpoint        (..)
    , elimIsEndpoint
    , BrowserVersion    (..)
    , TargetInfo        (..)
    , TargetId
    , endpoint

    , ClientApp
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
