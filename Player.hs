module Player where

import Prelude hiding (getLine)
import System.IO hiding (getLine)
import Control.Monad.Reader
import Control.Monad
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

type Player a = ReaderT PlayData IO a

data PlayData = PlayData {
  handle :: Handle,
  connId :: ConnId,
  world :: AcidState World,
  inputBuf :: MVar ByteString,
  die :: String -> IO ()
}

spawnPlayer :: PlayData -> Player () -> IO ThreadId
spawnPlayer pd pl = forkIO (runReaderT pl pd)

send' :: ByteString -> Player ()
send' bs = do
  h <- asks handle
  liftIO $ BS.hPut h bs

send :: Text -> Player ()
send txt = do
  h <- asks handle
  liftIO $ BS.hPut h (encodeUtf8 txt)

getLine :: Player ByteString
getLine = do
  buf <- readVar inputBuf
  h <- asks handle
  result <- liftIO (getLineBuf h buf)
  case result of
    ValidLine l buf' -> do
      writeVar inputBuf buf'
      return l
    NeedMore buf' -> do
      writeVar inputBuf buf'
      getLine
    Disconnect -> doDie "disconnect"
    TooLong _ -> doDie "input buffer limit reached"

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
  ans <- getLine
  playerDialog (cont ans)

