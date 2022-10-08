{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts #-}

module CDP.Handle where

import Data.Aeson (ToJSON)

import CDP.Internal.Runtime

data Handle ev where
    Handle :: FromJSONEvent ev => Handle' ev -> Handle ev

sendReceiveCommandResult :: forall a b ev. (Show a, ToJSON a, Command b) => Handle ev -> Maybe SessionId -> String -> Maybe a -> IO b
sendReceiveCommandResult (Handle h) = sendReceiveCommandResult' h

sendReceiveCommand :: (Show a, ToJSON a) => Handle ev -> Maybe SessionId -> String -> Maybe a -> IO ()
sendReceiveCommand (Handle h) = sendReceiveCommand' h
