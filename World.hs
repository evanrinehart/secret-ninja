{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module World where

{-
the world is stored in an acid state
queries and updates must happen in IO
-}

import Data.Maybe
import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Data.Time
import Control.Monad.State
import Control.Monad.Reader
import Data.Map
import qualified Data.Map as M

import YMap
import qualified YMap as Y
import IdWrappers
import Items
import RoomLink
import qualified RoomLink as RL
import Event
import GameTime
import TimeQueue
import qualified TimeQueue as TQ
import Mob
import Room
import Item

data World = World {
  mobs :: Map MobId Mob,
  rooms :: Map RoomId Room,
  items :: Map ItemId Item,

  mobLocations  :: YMap MobId RoomId,
  itemLocations :: YMap ItemId ItemLoc,
  roomLinks :: RoomLinkSet,

  eventQueueRT :: TimeQueue UTCTime Event,
  eventQueueGT :: TimeQueue GameTime Event
} deriving (Show, Typeable)

blankWorld :: World
blankWorld = World {
  mobs = M.singleton mobId0 mob0,
  rooms = M.singleton roomId0 room0,
  items = M.fromList [(itemId01,Item),(itemId02,Item),(itemId03,Item)],
  mobLocations = Y.empty,
  itemLocations = Y.fromList [
     (itemId01, InRoom roomId0),
     (itemId02, InRoom roomId0),
     (itemId03, InRoom roomId0)
    ],
  roomLinks = RL.empty,
  eventQueueRT = TQ.empty,
  eventQueueGT = TQ.empty
}

$(deriveSafeCopy 0 'base ''World)

queryState :: Query World World
queryState = ask

--testQ :: Query World World
--testQ = ask


testQ :: Query World [Item]
testQ = do
  y <- asks itemLocations
  im <- asks items
  let itemIds = Y.search (InRoom roomId0) y
  let items = catMaybes $ Prelude.map (flip M.lookup im) itemIds
  return items


$(makeAcidic ''World
  ['queryState
  ,'testQ])


loadWorld :: IO (AcidState World)
loadWorld = openLocalState blankWorld

{-
queryState0 :: Query WorldState0 WorldState0
queryState0 = ask

$(makeAcidic ''WorldState0 ['queryState0])
-}