{-# LANGUAGE OverloadedStrings #-}
module Misc where

import Data.ByteString.Char8
import qualified Data.ByteString as BS
import System.IO

data ReadConn =
  Disconnect |
  ValidLine ByteString ByteString |
  NeedMore ByteString |
  TooLong ByteString deriving (Show)

getLineBuf :: Handle -> ByteString -> IO ReadConn
getLineBuf h buf = do
  let bufSize = 10
  buf' <- BS.hGetSome h (bufSize - BS.length buf)
  let buf'' = BS.append buf buf'
  let (h,t) = BS.breakSubstring "\r\n" buf''
  return $ case () of
    () | BS.null buf' -> Disconnect
       | not (BS.null t) -> ValidLine h (BS.drop 2 t)
       | BS.length buf'' < bufSize -> NeedMore buf''
       | otherwise -> TooLong buf''

