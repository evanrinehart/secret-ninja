{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module World where

import Data.Acid
import Data.Time
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import System.Random

import WorldType
import Event
import EventAction
import TimeQueue
import qualified YMap as Y
import Item
import Room
import Rng
import IdWrappers
import Dice

queryState :: Query World World
queryState = ask

doEventsU :: UTCTime -> Update World ([EventOutput], Maybe UTCTime)
doEventsU now = do
  outs <- execWriterT $ do
    q <- gets eventQueue
    let (es, q') = getReadyEvents now q
    modify (\w -> w {eventQueue = q'})
    forM_ es (\(t,e) -> runEvent t e)
  maybeNext <- fmap nextTime (gets eventQueue)
  return (outs, maybeNext)

benchU :: Update World [Int]
benchU = do
  roll 5

{-
might be useful at this point to list all the updates which
would be needed at the IO <-> World interface.

execute events (used by the event thread)
the rest used by player threads
  enqueue event
  create object
  move object
  update player settings
  update object description
-}

setRandomSeedU :: Rng -> Rng -> Update World ()
setRandomSeedU mainRng auxRng = do
  modify $ \w -> w {
    justCreated = False,
    rng = MainRng mainRng,
    idGen = AuxRng auxRng
  }

$(makeAcidic ''World [
  'queryState,
  'doEventsU,
  'benchU,
  'setRandomSeedU])

nextEventTime :: AcidState World -> IO (Maybe UTCTime)
nextEventTime acid = do
  w <- queryWorld acid
  return (TimeQueue.nextTime (eventQueue w))

doEvents :: AcidState World -> IO ([EventOutput], Maybe UTCTime)
doEvents acid = do
  now <- getCurrentTime
  update acid (DoEventsU now)

queryWorld :: AcidState World -> IO World
queryWorld acid = query acid QueryState

load :: IO (AcidState World)
load = do
  putStrLn "WORLD: loading..."
  acid <- openLocalState blankWorld
  w <- queryWorld acid
  when (justCreated w) $ do
    putStrLn "WORLD: initializing random seed"
    g1 <- Rng.new
    g2 <- Rng.new
    update acid (SetRandomSeedU g1 g2)
  putStrLn "WORLD: loaded"
  return acid

close :: AcidState World -> IO ()
close acid = do
  putStrLn "CLOSING ACID STATE"
  createCheckpoint acid
  closeAcidState acid

