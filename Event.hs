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
  SendToAll ByteString deriving (Show,Typeable)

$(deriveSafeCopy 0 'base ''EventOutput)

{-

execEvent ::
  UTCTime ->
  Event ->
  WriterT [EventOutput] (Update World) ()
execEvent now e = case e of
  TestEvent -> do
    tell [SendToAll (encode "i love you")]
    q <- gets eventQueue
    let q' = schedule (addUTCTime 10 now) TestEvent q
    modify (\w -> w {eventQueue = q'})
  TestEvent2 (iid0,iid1,iid2,iid3) -> do
    maybeItemId <- abc
    case maybeItemId of
      Nothing -> globalMsg "an effect fizzles"
      Just itemId -> do
        globalMsg "Item explodes into several pieces!"
        loc <- whereIs itemId
        removeItem itemId
        createItem ItemFragment iid0 loc
        createItem ItemFragment iid1 loc
        createItem ItemFragment iid2 loc
        createItem BrokenItem iid3 loc
      
  _ -> tell [SendToAll (encode "unknown event")]

-}

