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
  rThread :: ThreadId,
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

withWriteLock :: IO () -> Conn -> IO ()
withWriteLock io (Conn _ lock _ _) = withMVar (const io) lock

write :: ByteString -> Conn -> IO ()
write raw (Conn _ _ h _) = BS.hPut h raw

read :: Conn -> Int -> IO ByteString
read (Conn _ _ h _) n = BS.hGetSome h n

killThread :: Conn -> IO ()
killThread (Conn tid _ _ _) = T.killThread tid

getLine :: Conn -> IO ByteString
getLine

getLineBuf :: Handle -> ByteString -> IO ReadConn
getLineBuf h buf = do
  let bufSize = 256
  buf' <- BS.hGetSome h (bufSize - BS.length buf)
  let buf'' = BS.append buf buf'
  let (h,t) = BS.breakSubstring "\r\n" buf''
  return $ case () of
    () | BS.null buf' -> Disconnect
       | not (BS.null t) -> ValidLine h (BS.drop 2 t)
       | BS.length buf'' < bufSize -> NeedMore buf''
       | otherwise -> TooLong buf''

