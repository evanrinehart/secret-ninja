{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Event where

import Data.SafeCopy
import Data.Typeable

import Data.ByteString

data Event =
  Checkpoint |
  Heal |
  Lightning |
  Spawn |
  TestEvent |
  TestEvent2 |
  GeneralUpdate deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Event)

data EventOutput = 
  SendToAll ByteString
    deriving (Show,Typeable)

$(deriveSafeCopy 0 'base ''EventOutput)


