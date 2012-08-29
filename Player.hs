module Player where

import Debug.Trace

import Data.Time
import Prelude hiding (getLine)
import System.IO hiding (getLine)
import Control.Monad.Reader
import Control.Monad
import Data.Monoid
import Data.Acid
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Text as T
import Data.Attoparsec
import Data.ByteString (ByteString)
import qualified Data.Map as M

import Mud
import World
import Conn hiding (getLine)
import qualified Conn (getLine)
import ConnSet (ConnSet)
import qualified ConnSet as CS
import Rainbow
import Rng
import Output
import Parsers

type Player a = ReaderT PlayData IO a
data PlayData = PlayData { plConn :: Conn, plMud :: Mud }

runPlayer :: PlayData -> IO ()
runPlayer pd = runReaderT login pd

disconnect :: String -> Player a
disconnect msg = do
  i <- asks (connId . plConn)
  liftIO $ do
    putStrLn ("PLAYER "++show i++": "++msg)
    myThreadId >>= killThread
    error "disconnect: you should not have reached this point"

rand :: (Int,Int) -> Player Int
rand range = do
  g <- asks (rng . plMud)
  liftIO $ withMVar g (Rng.randomR range)

getLine :: Player ByteString
getLine =
  myConn >>= liftIO . Conn.getLine >>= either disconnect return

login :: Player ()
login = do
  send "username: "
  username <- getLine
  password <- askForPassword "password: "
  testPrompt

testPrompt :: Player ()
testPrompt = do
  sendLock (send "> ")
  e <- fmap (parseOnly testCommand) getLine
  case e of
    Left problem -> do
      sendLock $ do
        send "wtf? "
        sendLn problem
      testPrompt
    Right c -> case c of
      Blank -> do
        testPrompt
      List -> do
        acid <- asks (world . plMud)
        l <- liftIO (query acid TestQ)
        sendLock (sendLn (show l))
        testPrompt
      End -> do
        sendLock (sendLn "goodbyte")
        disconnect "player typed quit"
      Gossip msg -> do
        conns <- asks (connections . plMud) >>= liftIO . CS.contents
        forM_ conns $ \conn -> do
          sendToLock (Left conn) $ \conn -> do
            sendToLn conn . color Cyan . mconcat $
              ["connection "
              ,show . connId $ conn
              ," gossips: "
              ,T.unpack msg]
        testPrompt
      StopServer -> shutdownServer

shutdownServer :: Player ()
shutdownServer = do
  signal <- asks (finalSignal . plMud)
  liftIO (putMVar signal ())

askForPassword :: String -> Player ByteString
askForPassword msg = do
  send msg
  asks plConn >>= liftIO . Conn.getPassword

sendTo :: Output a => Conn -> a -> Player ()
sendTo conn = liftIO . flip Conn.write conn . encode

sendToLn :: Output a => Conn -> a -> Player ()
sendToLn conn x = do
  sendTo conn x
  sendTo conn "\r\n"

send :: Output a => a -> Player ()
send x = myConn >>= flip sendTo x

sendLn :: Output a => a -> Player ()
sendLn x = myConn >>= flip sendToLn x

sendLock :: Player () -> Player ()
sendLock use = do
  conn <- myConn
  r <- ask
  liftIO $ withMVar (writeLock conn) (\_ -> runReaderT use r)

myConn :: Player Conn
myConn = asks plConn

lookupConn :: ConnId -> Player (Maybe Conn)
lookupConn cid = do
  mv <- asks (connections . plMud)
  liftIO $ withMVar mv (return . M.lookup cid)

sendToLock :: Either Conn ConnId -> (Conn -> Player ()) -> Player ()
sendToLock cOrCid use = do
  maybeConn <- either (return . Just) lookupConn cOrCid
  r <- ask
  case maybeConn of
    Nothing -> return ()
    Just conn -> liftIO $ do
      withMVar (writeLock conn) (\_ -> runReaderT (use conn) r)

