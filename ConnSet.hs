module ConnSet where

import Prelude hiding (lookup)
import Control.Monad
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

import Conn

type ConnSet = Map ConnId Conn

contents :: MVar ConnSet -> IO [Conn]
contents mv = fmap M.elems (readMVar mv)

new :: IO (MVar ConnSet)
new = newMVar (M.empty)

append :: MVar ConnSet -> Conn -> IO ()
append mv conn = modifyMVar_ mv (return . M.insert (connId conn) conn)

delete :: MVar ConnSet -> Integer -> IO ()
delete mv i = modifyMVar_ mv (return . M.delete i)

lookup :: ConnId -> MVar ConnSet -> IO (Maybe Conn)
lookup cid cs = fmap (M.lookup cid) (readMVar cs)

spawnConn :: MVar ConnSet -> Conn -> IO () -> IO ()
spawnConn cs conn io = void . forkIO . finally io $ do
  delete cs (connId conn)
  n <- fmap length (contents cs)
  putStrLn $ "conn thread finalizer: now "++show n++" conns left"

killConn :: MVar ConnSet -> ConnId -> IO ()
killConn cs cid = do
  conn <- lookup cid cs
  maybe (return ()) (hClose . chandle) conn

