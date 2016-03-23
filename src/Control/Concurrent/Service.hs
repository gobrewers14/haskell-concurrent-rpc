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
        Left  e      -> throwIO (e :: SomeException)
        Right result -> return result
    serverInterface requestMVar process = do
      (request, responseMVar) <- takeMVar requestMVar
      elr <- try (process request)
      putMVar responseMVar elr
      case elr of
        Left  e      -> throwIO e
        Right _      -> return ()
