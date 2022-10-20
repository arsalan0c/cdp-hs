{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Data.Default
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

import qualified CDP as CDP

main :: IO ()
main = do
    CDP.runClient def $ \handle -> do
        sessionId <- attachTarget handle
        subPageLoad handle sessionId

-- Attaches to target, returning the session id
attachTarget :: CDP.Handle -> IO T.Text
attachTarget handle = do
    -- get a target id
    targetInfo <- head . CDP.targetGetTargetsTargetInfos <$> 
        (CDP.sendCommandWait handle $ CDP.PTargetGetTargets Nothing)
    let targetId = CDP.targetTargetInfoTargetId targetInfo
    -- get a session id by attaching to the target
    CDP.targetAttachToTargetSessionId <$> do
        CDP.sendCommandWait handle $
            -- to enable sessions, flatten must be set to True
            CDP.PTargetAttachToTarget targetId (Just True)

-- | Subscribes to page load events for a given session
subPageLoad :: CDP.Handle -> T.Text -> IO ()
subPageLoad handle sessionId = do
    -- register a handler for the page load event 
    CDP.subscribeForSession handle sessionId (print . CDP.pageLoadEventFiredTimestamp)
    -- start receiving events
    CDP.sendCommandForSessionWait handle sessionId CDP.pPageEnable
    -- navigate to a page, triggering the page load event
    CDP.sendCommandForSessionWait handle sessionId $
        CDP.pPageNavigate "http://haskell.foundation"
    -- stop the program from terminating, to keep receiving events
    forever $ do
        threadDelay 1000