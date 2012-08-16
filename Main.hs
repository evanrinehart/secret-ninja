module Main where

import System.IO
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Acid
import Network
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map as M

import Rng
import Conn
import ConnSet
import WorldState0
import Player
import Misc

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
  v <- newIORef 0
  forever $ do
    (h,hostname,port) <- accept s
    hSetBuffering h NoBuffering
--    print hostname
--    print port
    i' <- readIORef v
    conn <- Conn.new h i'
    let pd = mkPlayData conn acid cs rng killServer
    ConnSet.append cs conn
    ConnSet.spawnConn cs conn (runPlayer pd)
--    putStrLn ("forked connection "++show i'++" thread "++show tid)
    modifyIORef v (+1)
