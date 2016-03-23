module Control.Concurrent.Service where

import Control.Exception
import Control.Concurrent.MVar

type ClientInterface req res = req -> IO res
type ServerInterface req res = (req -> IO res) -> IO ()

newService :: IO (ClientInterface req res, ServerInterface req res)
newService = do
  requestMVar <- newEmptyMVar
  return (clientInterface requestMVar, serverInterface requestMVar)
  where
    clientInterface requestMVar request = do
      responseMVar <- newEmptyMVar
      putMVar requestMVar (request, responseMVar)
      response <- readMVar responseMVar
      case response of
        Right result -> return result
        Left  e      -> throwIO (e :: SomeException)
    serverInterface requestMVar process = do
      (request, responseMVar) <- takeMVar requestMVar
      putMVar responseMVar =<< try (process request)
