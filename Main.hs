module Main where

import Mud
import Net
import EventThread

main :: IO ()
main = do
  mud <- Mud.load
  eventQueueThread mud
  listeningThread mud 4545
  Mud.closeOnFinalSignal mud
