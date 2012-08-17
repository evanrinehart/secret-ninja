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
import World
import Player
import Misc
import qualified Counter

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
    (h,hostname,port) <- accept s
    hSetBuffering h NoBuffering
--    print hostname
--    print port
    i <- Counter.take idSrc
    conn <- Conn.new h i
    let pd = mkPlayData conn acid cs rng killServer
    ConnSet.append cs conn
    ConnSet.spawnConn cs conn (runPlayer pd)
--    putStrLn $ "forked connection "++show i
