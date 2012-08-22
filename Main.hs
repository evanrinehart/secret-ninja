module Main where

import System.IO
import Control.Monad
import Control.Concurrent.MVar
import Data.Acid
import Network
import Control.Concurrent

import Rng
import Conn
import ConnSet
import World
import Player
import qualified Counter
import Data.Time

import Output
import TimeQueue
import Misc

main :: IO ()
main = do
  g <- newRng
  rng <- newMVar g
  world <- loadWorld
  finalSignal <- newEmptyMVar
  connSet <- ConnSet.new
  eventSig <- eventThread world connSet
  acceptConnections
    world connSet rng (putMVar finalSignal ()) (delayedSignal eventSig)
  takeMVar finalSignal
  closeAcidState world
  return ()

acceptConnections ::
  AcidState World ->
  MVar ConnSet ->
  MVar Rng ->
  IO () ->
  (NominalDiffTime -> IO ()) ->
  IO ()
acceptConnections acid cs rng killServer doEventsIn = void . forkIO $ do
  s <- listenOn (PortNumber 4545)
  idSrc <- Counter.new
  forever $ do
    (h,_,_) <- accept s -- handle, hostname, port
    hSetBuffering h NoBuffering
    i <- Counter.take idSrc
    conn <- Conn.new h i
    let pd = mkPlayData conn acid cs rng killServer doEventsIn
    ConnSet.append cs conn
    ConnSet.spawnConn cs conn (runPlayer pd)

eventThread :: AcidState World -> MVar ConnSet -> IO (MVar ())
eventThread acid cs = do
  sig <- newEmptyMVar
  forkIO . forever $ do
    takeMVar sig
    now <- getCurrentTime
    outs <- update acid (DoEventsU now)
    forM_ outs $ \o -> case o of
      SendToAll raw -> do
        conns <- contents cs
        forM_ conns $ \c -> do
          Conn.withLock c $ do
            Conn.write raw c
            Conn.write (encode "\r\n") c
      EventWake t -> do
        now <- getCurrentTime
        delayedSignal sig (diffUTCTime t now)
        return ()
  whenJust
    (query acid NextEventTimeQ)
    (\t -> do
      dt <- timeUntil t
      delayedSignal sig dt)
  return sig

delayedSignal :: MVar () -> NominalDiffTime -> IO ()
delayedSignal sig dt = void . forkIO $ do
  let us = ceiling (1000000 * dt) :: Integer
  if us > fromIntegral (maxBound :: Int)
    then return ()
    else do
      when (us > 0) (threadDelay (fromIntegral us))
      putMVar sig ()

timeUntil :: UTCTime -> IO NominalDiffTime
timeUntil t = do
  now <- getCurrentTime
  return (diffUTCTime t now)


