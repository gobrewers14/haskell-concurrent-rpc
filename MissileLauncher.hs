module MissileLauncher where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Service
import Data.Word
import System.Random

type Missile            = Word
type LaunchConfirmation = String

main :: IO ()
main = do
  (clientInterface, serverInterface) <- newService
  runMissileProduction clientInterface
    `race_` runLaunchSite serverInterface "Redmond"
    `race_` runLaunchSite serverInterface "Cambridge"

runLaunchSite :: ServerInterface Missile LaunchConfirmation -> String -> IO ()
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

runMissileProduction :: ClientInterface Missile LaunchConfirmation -> IO ()
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
