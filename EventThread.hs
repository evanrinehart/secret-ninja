module EventThread where

import Control.Concurrent
import Control.Monad

import Mud
import Conn
import Output
import World
import Misc
import Event
import ConnSet

eventQueueThread :: Mud -> IO ()
eventQueueThread mud = do
  putStrLn "EVENTS: begin"
  let w = world mud
  t <- World.nextEventTime w
  putStrLn $ "EVENTS: next event "++show t
  whenJust t (Mud.wakeAt mud)
  putStrLn "EVENTS: dispatch loop"
  forkIO . forever $ do
    Mud.waitForEventSignal mud
    putStrLn "EVENTS: event!"
    (outs, t) <- World.doEvents w
    forM_ outs (doOutput mud)
    whenJust t (Mud.wakeAt mud)
  return ()

doOutput :: Mud -> EventOutput -> IO ()
doOutput mud out = case out of
  SendToAll raw -> do
    conns <- ConnSet.contents (connections mud)
    forM_ conns $ \c -> do
      Conn.withLock c $ do
        Conn.write raw c
        Conn.write (encode "\r\n") c



