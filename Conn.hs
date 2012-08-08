module Conn where

-- connection

import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Concurrent as T
import Data.ByteString
import qualified Data.ByteString as BS

type ConnId = Integer

data Conn = Conn {
  handle :: Handle,
  writeLock :: MVar (), -- take to lock
  inputBuf :: MVar ByteString,
  connId :: ConnId,
}

data ReadConn =
  Disconnect |
  ValidLine ByteString ByteString |
  NeedMore ByteString |
  TooLong ByteString deriving (Show)

instance Show Conn where
  show = ("Connection "++) . show . connId

new :: Handle -> ConnId -> IO Conn
new h cid = do
  wl <- newMVar ()
  ibuf <- newMVar BS.empty
  return $ Conn {
    handle = h,
    writeLock = wl,
    inputBuf = ibuf,
    connId = cid
  }

write :: ByteString -> Conn -> IO ()
write raw conn = BS.hPut (handle conn) raw

read :: Conn -> Int -> IO ByteString
read conn n = BS.hGetSome (handle conn) n

getLine :: Conn -> IO (Either String ByteString)
getLine conn = do
  buf <- inputBuf conn
  result <- getLineBuf conn
  case result of
    Disconnect -> return (Left "remote host disconnected")
    TooLong _ -> return (Left "remote host sent a too-long line")
    ValidLine l buf' -> do
      updateBuffer conn buf'
      return (Right l)
    NeedMore buf' -> do
      updateBuffer conn buf'
      getLine conn

getLineBuf :: Conn -> ByteString -> IO ReadConn
getLineBuf conn buf = do
  let bufSize = 256
  buf' <- read conn (bufSize - BS.length buf)
  let buf'' = BS.append buf buf'
  let (h,t) = BS.breakSubstring "\r\n" buf''
  return $ case () of
    () | BS.null buf' -> Disconnect
       | not (BS.null t) -> ValidLine h (BS.drop 2 t)
       | BS.length buf'' < bufSize -> NeedMore buf''
       | otherwise -> TooLong buf''

updateBuffer :: Conn -> ByteString -> IO ()
updateBuffer conn buf = swapMVar (inputBuf conn) buf >> return ()
