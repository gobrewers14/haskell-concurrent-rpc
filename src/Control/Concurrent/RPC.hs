-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.RPC
-- Copyright   :  (c) Lars Petersen 2016
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--
-- This module offers the single method `newRPC` which creates a typed
-- communication channel. Give the one end of the channel to client threads
-- and the other end to worker threads.
--
-- Also see https://gist.github.com/lpeterse/da7b574da5c7a0dc9794 for an example.
-----------------------------------------------------------------------------
module Control.Concurrent.RPC where

import Control.Exception
import Control.Concurrent.MVar

-- | The client interface for threads invoking /remote procedure calls/.
--
--   * The operation blocks until another thread processed the request.
--   * If the other thread throws an exception during processing, the exception
--     is re-thrown in the thread waiting for the response.
--
-- > type Rocket  = String
-- > type Liftoff = Bool
-- >
-- > main = do
-- >   (launchRocket, withRocket) <- newRPC :: IO (RPC Rocket Liftoff, WithRPC Rocket Liftoff)
-- >   ..
-- >   catch
-- >     ( launchRocket "Apollo 11" >>= \liftoff-> if liftoff
-- >         then print "Houston, we have a liftoff!"
-- >         else print "Launch cancelled!"
-- >     )
-- >     ( \e-> print $ "Houston, we have a problem: " ++ show (e :: IOError) )
type RPC     request response = request -> IO response

-- | The interface for threads that serve and process /remote procedure calls/.
--
--   * More than one thread may be used to process requests all using the same
--     interface object. All processing threads will block on `Control.Concurrent.take`
--     on the same `MVar` and only one will be served at a time. Fairness
--     properties of `Control.Concurrent.MVar` apply.
--   * Exceptions thrown by the handler operation will be re-thrown in both
--     the processing thread and the requesting thread.
--
-- > type Rocket  = String
-- > type Liftoff = Bool
-- >
-- > main = do
-- >   (launchRocket, withRocket) <- newRPC :: IO (RPC Rocket Liftoff, WithRPC Rocket Liftoff)
-- >   ..
-- >   -- This is the rocket launch site thread. It forever waits for rockets and fires them into space one after the other.
-- >   forkIO $ withFile "/dev/null" WriteMode $ \space->
-- >     forever $ catch
-- >       ( withRocket $ \rocket-> do
-- >           weather <- getWeatherCondition
-- >           when (isGood weather) $
-- >             hPutStrLn space rocket -- The actual launch may throw an IOError!
-- >           return weather
-- >       )
-- >       ( \e-> print "A rocket exploded during launch phase: " ++ (e :: IOError) )
type WithRPC request response = (request -> IO response) -> IO ()

-- | Creates a new request-response communication channel that may be used
--   by arbitrary many requesting and/or processing threads.
--
-- > main :: IO ()
-- > main = do
-- >  (rpc, withRpc) <- newRPC
-- >  forkIO $ forever $ withRpc $ \request->
-- >    response <- doSomethingWith request
-- >    return response
-- >  response <- rpc request
-- >  ..
--
--   * `newRPC` initially creates one empty `Control.Concurrent.MVar.MVar`
--     for queueing requests.
--   * Each call of `rpc` creates a temporary `Control.Concurrent.MVar` for the
--     reponse.
--   * If the handler given to `withRpc` throws an exception, the exception is
--     re-thrown in the `withRpc` thread as well as in the `rpc` thread that
--     issued the call.
--   * If an `rpc` thread that issued a call dies before the request
--     processing has started, the request gets discarded. If processing has
--     already started, the processing will finish and the response gets discarded.
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
