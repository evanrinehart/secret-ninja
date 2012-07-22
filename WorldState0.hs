{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module WorldState0 where

{-
the world is stored in an acid state
queries and updates must happen in IO
-}

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

type WorldState = WorldState0

data WorldState0 = WorldState0 {
  mobs :: Map MobId Mob,
  rooms :: Map RoomId Room,
  items :: Map ItemId Item,

  mobLocations  :: YMap MobId RoomId,
  itemLocations :: YMap ItemId ItemLoc,
  roomLinks :: RoomLinkSet,

  eventQueueRT :: TimeQueue UTCTime Event,
  eventQueueGT :: TimeQueue GameTime Event
} deriving (Show, Typeable)

blankWorld :: UTCTime -> WorldState
blankWorld t0 = WorldState0 {
  mobs = M.singleton mobId0 mob0,
  rooms = M.singleton roomId0 room0,
  items = M.empty,
  mobLocations = Y.empty,
  itemLocations = Y.empty,
  roomLinks = RL.empty,
  eventQueueRT = TQ.empty,
  eventQueueGT = TQ.empty
}

$(deriveSafeCopy 0 'base ''WorldState0)

queryState :: Query WorldState WorldState
queryState = ask

$(makeAcidic ''WorldState0
  ['queryState])

{-
queryState0 :: Query WorldState0 WorldState0
queryState0 = ask

$(makeAcidic ''WorldState0 ['queryState0])
-}
