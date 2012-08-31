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

queryState :: Query World World
queryState = ask

testQ :: Query World [Item]
testQ = do
  y <- asks itemLocations
  im <- asks items
  let itemIds = Y.search (InRoom roomId0) y
  let items = catMaybes $ Prelude.map (flip M.lookup im) itemIds
  return items

doEventsU :: UTCTime -> Update World ([EventOutput], Maybe UTCTime)
doEventsU now = do
  outs <- execWriterT $ do
    q <- gets eventQueue
    let (es, q') = getReadyEvents now q
    modify (\w -> w {eventQueue = q'})
    forM_ es (\(t,e) -> runEvent t e)
  maybeNext <- fmap nextTime (gets eventQueue)
  return (outs, maybeNext)


$(makeAcidic ''World ['doEventsU, 'testQ, 'queryState])

nextEventTime :: AcidState World -> IO (Maybe UTCTime)
nextEventTime acid = do
  w <- query acid QueryState
  return (TimeQueue.nextTime (eventQueue w))

doEvents :: AcidState World -> IO ([EventOutput], Maybe UTCTime)
doEvents acid = do
  now <- getCurrentTime
  update acid (DoEventsU now)

load :: IO (AcidState World)
load = do
  putStrLn "WORLD: loading..."
  rng <- Rng.new
  putStrLn "WORLD: io generated rng"
  acid <- openLocalState (blankWorld rng)
  putStrLn "WORLD: loaded"
  return acid

close :: AcidState World -> IO ()
close acid = do
  putStrLn "CLOSING ACID STATE"
  createCheckpoint acid
  closeAcidState acid


randomId :: Update World RawId
randomId = fmap numbersToId ((replicateM 40 . randomRM) (0,15))

randomM :: Random a => Update World a
randomM = liftRandom random

randomRM :: Random a => (a,a) -> Update World a
randomRM range = liftRandom (randomR range)

liftRandom :: (Rng -> (a, Rng)) -> Update World a
liftRandom gen = do
  g <- gets rng
  let (x, g') = gen g
  modify (\w -> w {rng = g'})
  return x
