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
import Data.Text
import Data.Text.Encoding
import Data.Attoparsec

import WorldState0
import ConnSet
import Misc
import Dialog
import Rainbow

type Player a = ReaderT PlayData IO a

data PlayData = PlayData {
  myConn :: Conn,
  world :: AcidState World,
  connSet :: MVar ConnSet,
  die :: String -> IO (),
  killServer :: IO ()
}

spawnPlayer :: PlayData -> IO ThreadId
spawnPlayer pd = forkIO (runReaderT login pd)

login :: Player ()
login = do
  send "username: "
  username <- getLine
  password <- askForPassword "password: "
  commandLoop

commandLoop :: Player ()
commandLoop = forever $ do
  raw <- getLine
  let result = parseOnly commandParser0 raw
  case result of
    Left _ -> sendLn "WRONG"
    Right command -> case command of
      GMsg msg -> sendToAll msg
      Quit -> doDie "quitting"
    

askForPassword :: Text -> Player Text
askForPassword msg = do
  send msg
  send' "\255\251\1"
  Right password <- fmap (parseOnly telnetPassword) getLine'
  return password

telnetPassword :: Parser Text
telnetPassword = do
  hmm <- peekWord8
  case hmm of
    Nothing -> return ""
    Just w -> if w == 255
      then fmap (decodeUtf8 . BS.drop 3) takeByteString
      else fmap decodeUtf8 takeByteString

send' :: ByteString -> Player ()
send' bs = do
  h <- asks handle
  liftIO $ BS.hPut h bs

send :: Text -> Player ()
send txt = do
  h <- asks handle
  liftIO $ BS.hPut h (encodeUtf8 txt)

sendLn :: Text -> Player ()
sendLn txt = do
  send txt
  send crlf

sendToAll :: Text -> Player ()
sendToAll msg = do
  handles <- fmap (map fst) (asks connSet >>= readMVar)
  forM_ handles (sendTo msg)

sendToAllLn :: Text -> Player ()
sendToAllLn txt = do
  sendToAll txt
  sendToAll crlf

sendTo :: Text -> Handle -> Player ()
sendTo msg h = liftIO $ hPut h (encodeUtf8 msg)

--PUT THIS IN OTHER MODULE
colorize :: Color -> Text -> Text
colorize c txt = C.encode . C.color c

getLine' :: Player ByteString
getLine' = do
  result <- liftIO (getLineBuf h buf
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

