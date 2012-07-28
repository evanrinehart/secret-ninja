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

import ConnSet
import WorldState0
import Player
import Misc

main = do
  acid <- loadWorld
  mainWait <- newEmptyMVar
  let killServer = putMVar mainWait ()
  mvConns <- bootNetwork acid killServer
  
  void (takeMVar mainWait)

bootNetwork :: AcidState World -> IO () -> IO (MVar ConnSet)
bootNetwork acid killServer = do
  mvConns <- mkConnSet
  forkIO (acceptThread acid mvConns killServer)
  return mvConns

acceptThread ::
  AcidState World ->
  MVar ConnSet ->
  IO () ->
  IO ()
acceptThread acid mvConns killServer = do
  s <- listenOn (PortNumber 4545)
  v <- newIORef 0
  forever $ do
    (h,hostname,port) <- accept s
    hSetBuffering h NoBuffering
    print hostname
    print port
    i' <- readIORef v
    mvb <- newMVar BS.empty
    let pd = PlayData {
      handle = h,
      connId = i',
      world = acid,
      inputBuf = mvb,
      die = \msg -> do
        modifyMVar_ mvConns (return . M.delete i')
        putStrLn msg
        hClose h
        tid <- myThreadId
        killThread tid,
      killServer = killServer }
    conns <- takeMVar mvConns
    tid <- spawnPlayer pd
    putMVar mvConns (M.insert i' (h,tid) conns)
    putStrLn ("forked connection "++show i'++" thread "++show tid)
    modifyIORef v (+1)
