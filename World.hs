{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module World where

import Data.Acid
import Data.Time
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe

import WorldType
import Event
import TimeQueue
import qualified YMap as Y
import Item
import Room

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
    -- forM_ es (\(t,e) -> execEvent t e)
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
load = openLocalState blankWorld

close :: AcidState World -> IO ()
close acid = closeAcidState acid

