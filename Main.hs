module Main where

import Mud
import Net
import EventThread

main :: IO ()
main = do
  mud <- Mud.load
  eventQueueThread mud
  acceptConnections mud
  Mud.closeOnFinalSignal mud
