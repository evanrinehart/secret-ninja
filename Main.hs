module Main where

import Connection
import WorldState

main = do
--  (out, connSet) <- bootNetwork
  return ()

bootNetwork :: IO (MVar ConnSet)
bootNetwork = do
  (mvn, mvk, mvs) <- connSetThread
  forkIO (acceptThread out newConn delConn)
  return (out, s)

acceptThread ::
  ConnOutput ->
  NewConnInSet -> 
  DelConnFromSet -> 
  IO ()
acceptThread out newConn delConn = do
  s <- listenOn (PortNumber 4545)
  v <- newIORef 0
  forever $ do
    (h,hostname,port) <- accept s
    hSetBuffering h NoBuffering
    print hostname
    print port
    i' <- readIORef v
    tid <- forkIO $ connThread h
      (dieIO delConn h i')
      (connOutput out i')
    putStrLn ("forked connection "++show i'++" thread "++show tid)
    newConnInSet newConn i' h tid
    modifyIORef v (+1)
