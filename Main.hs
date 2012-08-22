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

main :: IO ()
main = do
  g <- newRng
  rng <- newMVar g
  world <- loadWorld
  finalSignal <- newEmptyMVar
  connSet <- ConnSet.new
  esig <- eventThread world connSet
  acceptConnections world connSet rng (putMVar finalSignal ())
  takeMVar finalSignal
  return ()

acceptConnections ::
  AcidState World ->
  MVar ConnSet ->
  MVar Rng ->
  IO () ->
  IO ()
acceptConnections acid cs rng killServer = const void =<< forkIO $ do
  s <- listenOn (PortNumber 4545)
  idSrc <- Counter.new
  forever $ do
    (h,_,_) <- accept s -- handle, hostname, port
    hSetBuffering h NoBuffering
--    print hostname
--    print port
    i <- Counter.take idSrc
    conn <- Conn.new h i
    let pd = mkPlayData conn acid cs rng killServer
    ConnSet.append cs conn
    ConnSet.spawnConn cs conn (runPlayer pd)
--    putStrLn $ "forked connection "++show i

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
  delayedSignal sig 0
  return sig

delayedSignal :: MVar () -> NominalDiffTime -> IO ()
delayedSignal sig dt = do
  forkIO $ do
    let us = ceiling (1000000 * dt)
    if dt < 0
      then error "negative delay"
      else if us > maxBound
        then return ()
        else do
          threadDelay us
          putMVar sig ()
  return ()
