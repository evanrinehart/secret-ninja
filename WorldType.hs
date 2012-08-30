{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module WorldType where

{-
the world is stored in an acid state
queries and updates must happen in IO
-}

import Data.Acid
import Data.Time
import qualified Data.Map as M
import Data.Map (Map)
import Data.Typeable
import Data.SafeCopy
import Control.Monad.Writer

import Conn (Conn, ConnId)
import qualified Conn
import Output
import Data.ByteString

import YMap
import qualified YMap as Y
import IdWrappers
import RoomLink (RoomLinkSet)
import qualified RoomLink as RL
import Event
import GameTime
import TimeQueue
import qualified TimeQueue as TQ
import Mob
import Room
import Item
import Rng

data World = World {
  rng :: Rng,

  mobs :: Map MobId Mob,
  rooms :: Map RoomId Room,
  items :: Map ItemId Item,

  mobLocations  :: YMap MobId RoomId,
  itemLocations :: YMap ItemId ItemLoc,
  roomLinks :: RoomLinkSet,

  eventQueue :: TimeQueue UTCTime Event
} deriving (Show, Typeable)

blankWorld :: Rng -> World
blankWorld rng = World {
  rng = rng,
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
  eventQueue = TQ
    [(read "2012-08-19 16:10:00 UTC", TestEvent)]
}

type EventUpdate = WriterT [EventOutput] (Update World)

$(deriveSafeCopy 0 'base ''World)



