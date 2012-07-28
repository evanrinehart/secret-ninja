module Main where

import Connection
import WorldState
import Player

main = do
  acid <- loadWorld
  mvConns <- bootNetwork
  return ()

bootNetwork :: AcidState World -> IO (MVar ConnSet)
bootNetwork acid = do
  mvConns <- connSetThread
  forkIO (acceptThread acid mvConns)
  return mvConns

acceptThread ::
  AcidState World ->
  MVar ConnSet -> 
  IO ()
acceptThread acid mvConns = do
  s <- listenOn (PortNumber 4545)
  v <- newIORef 0
  forever $ do
    (h,hostname,port) <- accept s
    hSetBuffering h NoBuffering
    print hostname
    print port
    i' <- readIORef v
    mvb <- emptyInputBuf
    let pl = PlayData
      {handle = h
      ,connId = i'
      ,world = acid
      ,inputBuf = mvb
      ,die = \msg -> do
         modifyMVar_ mvConns (M.delete i')
         putStrLn msg}
    conns <- takeMVar mvConns
    tid <- spawnPlayer pl
    putMVar mvConns (M.insert i' (h,tid) conns)
    putStrLn ("forked connection "++show i'++" thread "++show tid)
    modifyIORef v (+1)
