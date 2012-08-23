module ConnSet where

import Prelude hiding (lookup)
import Control.Monad
import System.IO
import Control.Exception
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as M

import Conn

type ConnSet = Map ConnId Conn

contents :: MVar ConnSet -> IO [Conn]
contents mv = fmap M.elems (readMVar mv)

new :: IO (MVar ConnSet)
new = newMVar (M.empty)

append :: MVar ConnSet -> Conn -> IO Int
append mv conn = modifyMVar mv $ \m -> do
  let m' = M.insert (connId conn) conn m
  let n = M.size m'
  return (m', n)

delete :: MVar ConnSet -> Integer -> IO Int
delete mv i = modifyMVar mv $ \m -> do
  let m' = M.delete i m
  let n = M.size m'
  return (m', n)

size :: MVar ConnSet -> IO Int
size mv = withMVar mv M.size

lookup :: ConnId -> MVar ConnSet -> IO (Maybe Conn)
lookup cid cs = fmap (M.lookup cid) (readMVar cs)

killConn :: MVar ConnSet -> ConnId -> IO ()
killConn cs cid = do
  conn <- lookup cid cs
  maybe (return ()) (hClose . chandle) conn

