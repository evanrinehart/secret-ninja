module ConnSet where

import Control.Monad
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

type ConnId = Integer
type ConnSet = Map ConnId (Handle, ThreadId)

listConnections :: MVar ConnSet -> IO [ConnId]
listConnections mv = fmap M.keys (readMVar mv)

connSetThread :: IO (
    MVar (ConnId,Handle,ThreadId),
    MVar ConnId,
    MVar ConnSet
  )
connSetThread = do
  mvs <- newMVar (M.empty)
  mvn <- newEmptyMVar
  mvk <- newEmptyMVar
  forkIO . forever $ do
    (i,h,tid) <- takeMVar mvn
    modifyMVar_ mvs (return . M.insert i (h,tid))
    putStrLn "connset new"
    return ()
  forkIO . forever $ do
    i <- takeMVar mvk
    modifyMVar_ mvs (return . M.delete i)
    putStrLn "connset remove"
    return ()
  return (mvn, mvk, mvs)
