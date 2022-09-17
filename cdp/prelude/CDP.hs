{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CDP (Handle, Config(..), module CDP, module CDP.Protocol) where

import Data.Proxy

import CDP.Internal.Runtime
import CDP.Protocol

type ClientApp b = Handle Event -> IO b

runClient   :: Config -> ClientApp a -> IO a
runClient config app = runClient' config app

subscribe   :: forall a. FromEvent Event a => Handle Event -> (a -> IO ()) -> IO ()
subscribe handle h = subscribe' handle h

unsubscribe :: forall a. FromEvent Event a => Handle Event -> Proxy a -> IO ()
unsubscribe handle p = unsubscribe' handle p