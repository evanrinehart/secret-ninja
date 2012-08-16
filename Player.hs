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
import qualified Data.Map as M

import WorldState0
import Conn hiding (getLine)
import qualified Conn (getLine)
import ConnSet (ConnSet)
import qualified ConnSet as CS
import Misc
import Dialog
import Rainbow
import Rng
import Output

type Player a = ReaderT PlayData IO a

data PlayData = PlayData {
  myConn :: Conn,
  world :: AcidState World,
  connSet :: MVar ConnSet,
  die :: String -> IO (),
  rng :: MVar Rng,
  killServer :: IO ()
}

mkPlayData ::
  Conn ->
  AcidState World ->
  MVar ConnSet ->
  MVar Rng ->
  IO () -> PlayData
mkPlayData c acid cs g k =
  PlayData {
    myConn = c,
    world = acid,
    connSet = cs,
    die = mkDie cs (connId c) (chandle c),
    rng = g,
    killServer = k
  }

mkDie :: MVar ConnSet -> ConnId -> Handle -> String -> IO ()
mkDie cs cid h msg = do
  modifyMVar_ cs (return . M.delete cid)
  putStrLn msg
  hClose h
  tid <- myThreadId
  killThread tid

runPlayer :: PlayData -> IO ()
runPlayer pd = runReaderT login pd

rand :: (Int,Int) -> Player Int
rand range = do
  g <- asks rng
  liftIO $ withMVar g (Rng.randomR range)

login :: Player ()
login = do
  send "username: "
  username <- getLine
  password <- askForPassword "password: "
  sendLn "fuck all yall"
  send "one last question: "
  _ <- getLine
  disconnect "test over, disconnecting"

disconnect :: String -> Player a
disconnect msg = do
  io <- asks die
  liftIO (io msg)
  error "die didnt"

askForPassword :: String -> Player Text
askForPassword msg = do
  send msg
  send "\255\251\1"
  Right password <- fmap (parseOnly telnetPassword) getLine
  return password

sendTo :: Output a => Conn -> a -> Player ()
sendTo conn = liftIO . flip Conn.write conn . encode

sendToLn :: Output a => Conn -> a -> Player ()
sendToLn conn x = do
  sendTo conn x
  sendTo conn "\r\n"

send :: Output a => a -> Player ()
send x = asks myConn >>= flip sendTo x

sendLn :: Output a => a -> Player ()
sendLn x = asks myConn >>= flip sendToLn x

sendLock :: (Conn -> Player ()) -> Player ()
sendLock use = do
  conn <- asks myConn
  r <- ask
  liftIO $ withMVar (writeLock conn) (\_ -> runReaderT (use conn) r)

sendToLock :: ConnId -> (Conn -> Player ()) -> Player ()
sendToLock cid use = do
  r <- ask
  mv <- asks connSet
  conny <- liftIO $ withMVar mv (return . M.lookup cid)
  case conny of
    Nothing -> return ()
    Just conn -> liftIO $ do
      withMVar (writeLock conn) (\_ -> runReaderT (use conn) r)

telnetPassword :: Parser Text
telnetPassword = do
  hmm <- peekWord8
  case hmm of
    Nothing -> return (T.empty)
    Just w -> if w == 255
      then fmap (decodeUtf8 . BS.drop 3) takeByteString
      else fmap decodeUtf8 takeByteString

getLine :: Player ByteString
getLine = do
  conn <- asks myConn
  hmm <- liftIO (Conn.getLine conn)
  case hmm of
    Left reason -> disconnect reason
    Right x -> return x

