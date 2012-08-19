module Main where

import System.IO
import Control.Monad
import Control.Concurrent.MVar
import Data.Acid
import Network

import Rng
import Conn
import ConnSet
import World
import Player
import qualified Counter

main :: IO ()
main = do
  g <- newRng
  rng <- newMVar g
  world <- loadWorld
  finalSignal <- newEmptyMVar
  connSet <- ConnSet.new
  -- start event processing thread
  acceptConnections world connSet rng (putMVar finalSignal ())
  takeMVar finalSignal
  return ()

acceptConnections ::
  AcidState World ->
  MVar ConnSet ->
  MVar Rng ->
  IO () ->
  IO ()
acceptConnections acid cs rng killServer = do
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
