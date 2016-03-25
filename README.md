concurrent-rpc
==============

[![Available on Hackage][badge-hackage]][hackage]
[![License MIT][badge-license]][license]
[![Build Status][badge-travis]][travis]

### Summary

This library is small wrapper around `Control.Concurrent.MVar.MVar`s that can
be used to implement request-response communication between different threads.

### Example

```haskell
module MissileLauncher where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.RPC
import Data.Word
import System.Random

type Missile    = Word
type LaunchSite = String

main :: IO ()
main = do
  (launchMissile, withMissile) <- newRPC
  runMissileProduction launchMissile
    `race_` runLaunchSite withMissile "Redmond"
    `race_` runLaunchSite withMissile "Cambridge"

runLaunchSite :: WithRPC Missile LaunchSite -> LaunchSite -> IO ()
runLaunchSite withMissile site = forever $ do
  sleepRandom
  catch
    ( withMissile $ \missile-> do
        r <- random100
        if r < 10
          then error $ "bad weather in " ++ site
          else do
            printThread $ site ++ ": LAUNCH THE MISSILE!"
            return site
    )
    ( \e-> do
      let _ = e :: SomeException
      printThread $ site ++ ": Couldn't launch. Waiting for next missile."
    )

runMissileProduction :: RPC Missile LaunchSite -> IO ()
runMissileProduction launchMissile =
  produce  `race_` produce `race_` produce `race_` produce
  where
    produce = forever $ do
      sleepRandom
      missile <- randomIO :: IO Missile
      catch
        ( do
            printThread $ "Production: Ready to launch missile " ++ show missile
            site <- launchMissile missile
            printThread $ "Production: Missile " ++ show missile ++ " launched in " ++ site
        )
        ( \e->
            printThread $ "Production: Missile " ++ show missile ++
                          " failed to launch due to " ++ show (e :: SomeException)
        )

printThread :: Show a => a -> IO ()
printThread x = do
  threadId <- myThreadId
  random100 >>= \x-> threadDelay (x * 100)
  putStrLn $ show threadId ++ ": " ++ show x

random100 :: IO Int
random100 = (`mod` 100) <$> randomIO

sleepRandom :: IO ()
sleepRandom = random100 >>= \x-> threadDelay (x * 100000)
```

### Dependencies

   - base >= 4.7 && < 5

[badge-travis]: https://img.shields.io/travis/lpeterse/haskell-concurrent-rpc.svg
[travis]: https://travis-ci.org/lpeterse/haskell-concurrent-rpc
[badge-hackage]: https://img.shields.io/hackage/v/concurrent-rpc.svg?dummy
[hackage]: https://hackage.haskell.org/package/concurrent-rpc
[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?dummy
[license]: https://github.com/lpeterse/haskell-concurrent-rpc/blob/master/LICENSE
[issues]: https://github.com/lpeterse/haskell-concurrent-rpc/issues
[Github]: https://github.com/lpeterse/haskell-concurrent-rpc
