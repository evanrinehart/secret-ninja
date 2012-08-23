module WorldQ where

import Data.Acid
import Data.Time

import World

queryState :: Query World World
queryState = ask

testQ :: Query World [Item]
testQ = do
  y <- asks itemLocations
  im <- asks items
  let itemIds = Y.search (InRoom roomId0) y
  let items = catMaybes $ Prelude.map (flip M.lookup im) itemIds
  return items

$(makeAcidic ''World ['queryState, 'testQ])

nextEventTime :: AcidState World -> IO (Maybe UTCTime)
nextEventTime acid = do
  w <- query acid QueryState
  return (TimeQueue.nextTime (eventQueue w))
