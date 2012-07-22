{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module WorldState0 where

{-
the world is stored in an acid state
queries and updates must happen in IO
-}

import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader
import Data.Map

import YMap
import IdWrappers
import Items
import RoomLink
import Event
import TimeQueue
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

  eventQueue :: TimeQueue Event
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''WorldState0)

writeState :: WorldState -> Update WorldState ()
writeState w = put w

writeY :: YMap ItemId ItemLoc -> Update WorldState ()
writeY s = do
  w <- get
  put (w {itemLocations = s})

queryState :: Query WorldState WorldState
queryState = ask

queryY :: Query WorldState RoomLinkSet
queryY = do
  w <- ask
  return (roomLinks w)

$(makeAcidic ''WorldState0
  ['writeState, 'queryState, 'queryY, 'writeY])
{-
queryState0 :: Query WorldState0 WorldState0
queryState0 = ask

$(makeAcidic ''WorldState0 ['queryState0])
-}
