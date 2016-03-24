-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.RPC
-- Copyright   :  (c) Lars Petersen 2016
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--
-- /Example1:/ Use `RPC` to synchronise resource access (like printing to the
-- terminal) of concurrent threads. Compare the `RPC` solution
--
-- > import Control.Monad (forever)
-- > import Control.Concurrent
-- > import Control.Concurrent.RPC
-- >
-- > main :: IO ()
-- > main = do
-- >   (log, withLogMessage) <- newRPC
-- >   forkIO $ forever $ log "<<<<<<<"
-- >   forkIO $ forever $ log ">>>>>>>"
-- >   forever $ withLogMessage print
-- >
-- > -- Example output:
-- > --
-- > -- "<<<<<<<"
-- > -- ">>>>>>>"
-- > -- "<<<<<<<"
-- > -- ">>>>>>>"
--
-- with this counter example which has a little issue with concurrency:
--
-- > import Control.Monad (forever)
-- > import Control.Concurrent
-- >
-- > main :: IO ()
-- > main = do
-- >   forkIO $ forever $ print "<<<<<<<"
-- >   forever $ print ">>>>>>>"
-- >
-- > -- Example output:
-- > --
-- > -- ""><><><><><><><""
-- > --
-- > -- ""><><><><><><><""
-- > --
-- > -- ""><><><><><><><""
-----------------------------------------------------------------------------
module Control.Concurrent.RPC where

import Control.Exception
import Control.Concurrent.MVar

type RPC     request response = request -> IO response
type WithRPC request response = (request -> IO response) -> IO ()

newRPC :: IO (RPC request response, WithRPC request response)
newRPC = do
  requestMVar <- newEmptyMVar
  return (createRequest requestMVar, processRequest requestMVar)
  where
    createRequest requestMVar request = do
      responseMVar <- newEmptyMVar
      putMVar requestMVar (request, responseMVar)
      response <- readMVar responseMVar
      case response of
        Left  e      -> throwIO (e :: SomeException)
        Right result -> return result
    processRequest requestMVar process = do
      (request, responseMVar) <- takeMVar requestMVar
      response <- try (process request)
      putMVar responseMVar response
      case response of
        Left  e      -> throwIO (e :: SomeException)
        Right _      -> return ()
