{-# LANGUAGE OverloadedStrings #-}
module Conn where

-- connection

import Data.IORef
import Prelude hiding (getLine, read)
import System.IO hiding (getLine)
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Concurrent as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS

import Output

type ConnId = Integer

data Conn = Conn {
  chandle :: Handle,
  writeLock :: MVar (), -- take to lock
  inputBuf :: IORef ByteString,
  connId :: ConnId
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
  ibuf <- newIORef BS.empty
  return $ Conn {
    chandle = h,
    writeLock = wl,
    inputBuf = ibuf,
    connId = cid
  }

write :: Output a => a -> Conn -> IO ()
write raw conn = BS.hPut (chandle conn) (encode raw)

read :: Conn -> Int -> IO ByteString
read conn n = BS.hGetSome (chandle conn) n

getLine :: Conn -> IO (Either String ByteString)
getLine conn = do
  buf <- readIORef (inputBuf conn)
  result <- getLineBuf conn buf
  case result of
    Disconnect -> return (Left "remote host disconnected")
    TooLong buf' -> do
      writeIORef (inputBuf conn) BS.empty
      return (Right buf')
    ValidLine l buf' -> do
      writeIORef (inputBuf conn) buf'
      return (Right l)
    NeedMore buf' -> do
      writeIORef (inputBuf conn) buf'
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

