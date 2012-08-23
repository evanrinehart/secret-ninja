module EventThread where

import Control.Concurrent
import Control.Monad

import Mud
import World
import Conn
import Output

eventQueueThread :: Mud -> IO ()
eventQueueThread mud = do
  let w = world mud
  t <- WorldQ.nextEventTime w
  whenJust t (Mud.wakeAt mud)
  forkIO . forever $ do
    (outs, t) <- WorldU.doEvents w
    forM_ outs doOutput
    whenJust t (Mud.wakeAt mud)

doOutput :: EventOutput -> IO ()
doOutput out = case out of
  SendToAll raw -> do
    conns <- contents cs
    forM_ conns $ \c -> do
      Conn.withLock c $ do
        Conn.write raw c
        Conn.write (encode "\r\n") c



