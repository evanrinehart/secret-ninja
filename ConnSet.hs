module ConnSet where

import Control.Monad
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

type ConnSet = Map ConnId Conn

contents :: MVar ConnSet -> IO [Conn]
contents mv = fmap M.elems (readMVar mv)

new :: IO (MVar ConnSet)
new = newMVar (M.empty)

append :: MVar ConnSet -> Integer -> Handle -> ThreadId -> IO ()
append mv i h tid = modifyMVar_ mv (return . M.insert i (h,tid))

delete :: MVar ConnSet -> Integer -> IO ()
delete mv i = modifyMVar_ mv (return . M.delete i)

lookup :: ConnId -> MVar ConnSet -> IO (Maybe Conn)
lookup cid mvs = fmap (M.lookup cid cs) readMVar mvs

spawnConn :: MVar ConnSet -> Conn -> IO () -> IO ()
spawnConn cs c io = forkIO (finally io (CS.delete cs (connId c)))

killConn :: MVar ConnSet -> ConnId -> IO ()
killConn cs cid = do
  conn <- CS.lookup cs cid
  maybe (return ()) (hClose . handle) conn

