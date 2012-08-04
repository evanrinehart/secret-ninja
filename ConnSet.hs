module ConnSet where

import Control.Monad
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

type ConnSet = Map ConnId Conn

listConnections :: MVar ConnSet -> IO [ConnId]
listConnections mv = fmap M.keys (readMVar mv)

mkConnSet :: IO (MVar ConnSet)
mkConnSet = newMVar (M.empty)

addToConnSet :: MVar ConnSet -> Integer -> Handle -> ThreadId -> IO ()
addToConnSet mv i h tid = modifyMVar_ mv (return . M.insert i (h,tid))

delFromConnSet :: MVar ConnSet -> Integer -> IO ()
delFromConnSet mv i = modifyMVar_ mv (return . M.delete i)

lookupConn :: ConnId -> MVar ConnSet -> IO (Maybe Conn)
lookupConn cid mvs = fmap (M.lookup cid cs) readMVar mvs

