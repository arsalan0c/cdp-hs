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
    , SomeEndpoint      (..)
    , fromSomeEndpoint
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
    , subscribeForSession
    , unsubscribe

    , Command (..)
    , SomeCommand (..)
    , Promise (..)
    , fromSomeCommand
    , readPromise
    , sendCommand
    , sendCommandForSession
    , sendCommandWait
    , sendCommandForSessionWait

    , module CDP.Domains
    ) where

import Data.Proxy (Proxy)

import CDP.Domains
import CDP.Runtime
