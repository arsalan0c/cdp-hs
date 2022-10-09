{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts #-}

module CDP.Handle
    ( Handle
    , sendReceiveCommandResult
    , sendReceiveCommand
    ) where

import Data.Aeson (ToJSON)

import CDP.Internal.Runtime

sendReceiveCommandResult :: forall a b ev. (Show a, ToJSON a, Command b) => Handle -> String -> Maybe a -> IO b
sendReceiveCommandResult = sendReceiveCommandResult'

sendReceiveCommand :: (Show a, ToJSON a) => Handle -> String -> Maybe a -> IO ()
sendReceiveCommand = sendReceiveCommand'
