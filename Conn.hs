{-# LANGUAGE OverloadedStrings #-}
module Conn where

-- connection

import Data.IORef
import Prelude hiding (getLine, read)
import System.IO hiding (getLine)
import Control.Concurrent.MVar
import qualified Control.Concurrent as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import Control.Exception

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
write raw conn = do
  _ <- try $ BS.hPut (chandle conn) (encode raw) :: IO (Either IOException ())
  return ()

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
      return . Right . stripTelnet $ buf'
    ValidLine l buf' -> do
      writeIORef (inputBuf conn) buf'
      return . Right . stripTelnet $ l
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

stripTelnet :: ByteString -> ByteString
stripTelnet inp = case BS.split 255 inp of
  [] -> BS.empty
  [clean] -> clean
  x:xs -> BS.append x fixes where
    fixes = BS.concat . map chopProto $ xs
    chopProto bs = case BS.uncons bs of
      Nothing -> BS.empty
      Just (w,ws) -> if w `elem` [251,252,253,254]
        then BS.drop 1 ws
        else ws -- and be damned

getPassword :: Conn -> IO ByteString
getPassword conn = do
  let h = chandle conn
  BS.hPut h "\255\251\1"
  e <- getLine conn
  case e of
    Left problem -> error problem
    Right pass -> do
      BS.hPut h "\255\252\1\r\n"
      return pass

