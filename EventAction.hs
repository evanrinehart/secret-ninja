module EventAction where

import Control.Monad.Writer
import Control.Monad.State
import Data.Time

import WorldType
import Event
import Output

{-
in the monad EventUpdate a
get reads the world state
put and modify writes the world state
tell emits an output like a message to a player
ask produces the next random id
-}

{-
runEvent :: UTCTime -> Event -> EventUpdate ()
runEvent now e = case e of
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

runEvent :: UTCTime -> Event -> EventUpdate ()
runEvent _ _ = return ()
