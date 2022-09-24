{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts #-}

module CDP.Handle where

import Data.Aeson (ToJSON)

import CDP.Internal.Runtime

data Handle ev where
    Handle :: FromJSONEvent ev => Handle' ev -> Handle ev

sendReceiveCommandResult :: forall a b ev. (Show a, ToJSON a, Command b) => Handle ev -> String -> Maybe a -> IO (Either Error b)
sendReceiveCommandResult (Handle h) = sendReceiveCommandResult' h

sendReceiveCommand :: (Show a, ToJSON a) => Handle ev -> String -> Maybe a -> IO (Maybe Error)
sendReceiveCommand (Handle h) = sendReceiveCommand' h
