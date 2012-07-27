{-# LANGUAGE OverloadedStrings #-}
module Connection where

import Control.Monad
import System.IO
import Network
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString (hGetSome)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.IORef

{-
use masterSpawn to boot up the networking stuff
you will get a ConnOutput and a MVar ConnSet to
play with in the main thread.

listConnections is meant to be used to inspect
the currently existing connections.
-}

data ReadConn =
  Disconnect |
  ValidLine ByteString ByteString |
  NeedMore ByteString |
  TooLong ByteString deriving (Show)

type ConnId = Integer
type ConnSet = Map ConnId (Handle, ThreadId)

-- in this module, conn output is coming INTO the server
newtype ConnOutput = ConnOutput (MVar (ConnId, ByteString))
newtype NewConnInSet = NewConnInSet (MVar (ConnId, Handle, ThreadId))
newtype DelConnFromSet = DelConnFromSet (MVar ConnId)

--public api
bootNetwork :: IO (ConnOutput, MVar ConnSet)
bootNetwork = do
  out <- fmap ConnOutput newEmptyMVar
  (newConn, delConn, s) <- connSetThread
  forkIO (acceptThread out newConn delConn)
  return (out, s)

listConnections :: MVar ConnSet -> IO [ConnId]
listConnections mv = fmap M.keys (readMVar mv)


-- internal stuff
--
connOutput :: ConnOutput -> ConnId -> ByteString -> IO ()
connOutput (ConnOutput mvo) n bs = putMVar mvo (n, bs)

newConnInSet :: NewConnInSet -> ConnId -> Handle -> ThreadId -> IO ()
newConnInSet (NewConnInSet mvn) n h tid = putMVar mvn (n,h,tid)

delConnFromSet :: DelConnFromSet -> ConnId -> IO ()
delConnFromSet (DelConnFromSet mvk) n = putMVar mvk n



getLineBuf :: Handle -> ByteString -> IO ReadConn
getLineBuf h buf = do
  let bufSize = 10
  buf' <- hGetSome h (bufSize - BS.length buf)
  let buf'' = BS.append buf buf'
  let (h,t) = BS.breakSubstring "\r\n" buf''
  return $ case () of
    () | BS.null buf' -> Disconnect
       | not (BS.null t) -> ValidLine h (BS.drop 2 t)
       | BS.length buf'' < bufSize -> NeedMore buf''
       | otherwise -> TooLong buf''

connThread :: Handle -> IO () -> (ByteString -> IO ()) -> IO ()
connThread h die out = loop BS.empty where
  loop buf = do
    putStrLn "connthread looped..."
    rc <- getLineBuf h buf
    print rc
    case rc of
      Disconnect -> die
      ValidLine l buf' -> do
        out l
        loop buf'
      NeedMore buf' -> loop buf'
      TooLong buf' -> die
    

dieIO :: DelConnFromSet -> Handle -> ConnId -> IO ()
dieIO kill h i = do
  putStrLn (show i ++ " is dying")
  hClose h
  delConnFromSet kill i

connSetThread :: IO (NewConnInSet, DelConnFromSet, MVar ConnSet)
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
  return (NewConnInSet mvn, DelConnFromSet mvk, mvs)

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

