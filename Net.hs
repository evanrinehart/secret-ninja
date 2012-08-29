module Net where

import System.IO
import Control.Monad
import Control.Concurrent.MVar
import Data.Acid
import Network
import Control.Concurrent
import Control.Exception

import Conn
import ConnSet
import World
import Player
import qualified Counter
import Data.Time
import Mud

listeningThread :: Mud -> Int -> IO ()
listeningThread mud port = void . forkIO $ do
  s <- listenOn (PortNumber (fromIntegral port))
  idSrc <- Counter.new
  forever $ do
    (h,_,_) <- accept s -- handle, hostname, port
    hSetBuffering h NoBuffering
    i <- Counter.take idSrc
    conn <- Conn.new h i
    spawnConn conn mud

spawnConn :: Conn -> Mud -> IO ()
spawnConn conn mud = forkIO (finally io cleanUp) >> return () where
  cs = connections mud
  io = do
    n <- ConnSet.append cs conn
    putStrLn ("NET: new connection, now "++show n++" conns")
    runPlayer (PlayData conn mud)
  cleanUp = do
    n <- ConnSet.delete cs (connId conn)
    hClose (chandle conn)
    putStrLn ("NET: connection terminated, now "++show n++" conns left")
