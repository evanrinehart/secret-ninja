module Mud where

-- looking at the servers top level IO stuff

import Data.Acid
import Control.Concurrent.MVar

import qualified Rng
import qualified ConnSet
import qualified World

data Mud = Mud {
  world :: AcidState World,
  rng :: MVar Rng,
  connections :: MVar ConnSet,
  finalSignal :: MVar (), -- put here to kill server
  eventSignal :: MVar (), -- put here to wake up event queue
}

load :: IO Mud
load = do
  rng <- Rng.new
  world <- World.load
  conns <- ConnSet.new
  die <- newEmptyMVar
  wake <- newEmptyMVar
  return (Mud world rng conns die wake)

closeOnFinalSignal :: Mud -> IO ()
closeOnFinalSignal mud = do
  takeMVar (finalSignal mud)
  putStrLn "SERVER: final signal arrived, closing world"
  World.close (world mud)
  putStrLn "SERVER: end of program"

wakeAt :: UTCTime -> Mud -> IO ()
wakeAt mud t = do
  now <- getCurrentTime
  wakeIn (diffUTCTime t now) mud

wakeIn :: NominalDiffTime -> Mud -> IO ()
wakeIn dt mud = void . forkIO $ do
  let us = ceiling (1000000 * dt) :: Integer
  if us > fromIntegral (maxBound :: Int)
    then return ()
    else do
      when (us > 0) (threadDelay (fromIntegral us))
      putMVar (eventSignal mud) ()

