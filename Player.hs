{-# LANGUAGE OverloadedStrings #-}
module Player where

import Prelude hiding (getLine)
import System.IO hiding (getLine)
import Control.Monad.Reader
import Control.Monad
import Data.Monoid
import Data.Acid
import Control.Concurrent.MVar
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding

import WorldState0
import ConnSet
import Misc
import Dialog
import CText
import qualified CText as C

type Player a = ReaderT PlayData IO a

data PlayData = PlayData {
  handle :: Handle,
  connId :: ConnId,
  world :: AcidState World,
  inputBuf :: MVar ByteString,
  die :: String -> IO (),
  killServer :: IO ()
}

spawnPlayer :: PlayData -> IO ThreadId
spawnPlayer pd = forkIO (runReaderT login pd)

login :: Player ()
login = do
  send "fuck you dude> "
  ans <- getLine
  sendColor BrightGreen ("you just said: " <> ans <> crlf)
  send "again, fuck you. good bye\r\n"
  doDie "connection terminated"

send' :: ByteString -> Player ()
send' bs = do
  h <- asks handle
  liftIO $ BS.hPut h bs

send :: Text -> Player ()
send txt = do
  h <- asks handle
  liftIO $ BS.hPut h (encodeUtf8 txt)

sendColor :: Color -> Text -> Player ()
sendColor c txt = send $ C.encode (C.color c txt)

getLine' :: Player ByteString
getLine' = do
  buf <- readVar inputBuf
  h <- asks handle
  result <- liftIO (getLineBuf h buf)
  case result of
    ValidLine l buf' -> do
      writeVar inputBuf buf'
      return l
    NeedMore buf' -> do
      writeVar inputBuf buf'
      getLine'
    Disconnect -> doDie "disconnect"
    TooLong _ -> doDie "input buffer limit reached"

getLine :: Player Text
getLine = fmap decodeUtf8 getLine'

writeVar :: (PlayData -> MVar a) -> a -> Player ()
writeVar field x = do
  mv <- asks field
  liftIO (putMVar mv x)

readVar :: (PlayData -> MVar a) -> Player a
readVar field = do
  mv <- asks field
  liftIO (takeMVar mv)

doDie :: String -> Player a
doDie msg = do
  i <- asks connId
  doIt <- asks die
  let finalMsg = "connection "++show i++" dead: "++msg
  liftIO (doIt finalMsg) -- should kill this thread
  error "doDie: this should not have been executed"

playerDialog :: Dialog ByteString a -> Player a
playerDialog (Answer x) = return x
playerDialog (Question q cont) = do
  send' q
  ans <- getLine'
  playerDialog (cont ans)

