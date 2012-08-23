module WorldU where

import Data.Acid
import Data.Time
import Control.Monad.Writer

import World
import TimeQueue

doEvents :: AcidState World -> IO ([EventOutput], Maybe UTCTime)
doEvents acid = do
  now <- getCurrentTime
  update acid (DoEventsU now)

doEventsU :: UTCTime -> Update World ([EventOutput], Maybe UTCTime)
doEventsU now = do
  outs <- execWriterT $ do
    q <- gets eventQueue
    let (es, q') = getReadyEvents now q
    modify (\w -> w {eventQueue = q'})
    forM_ es (\(t,e) -> execEvent t e)
  maybeNext <- fmap nextTime (gets eventQueue)
  return (outs, maybeNext)

$(makeAcidic ''World ['doEventsU])


