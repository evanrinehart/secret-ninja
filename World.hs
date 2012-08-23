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
import Control.Monad.Reader
import Data.Map
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.State

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

data World = World {
  mobs :: Map MobId Mob,
  rooms :: Map RoomId Room,
  items :: Map ItemId Item,

  mobLocations  :: YMap MobId RoomId,
  itemLocations :: YMap ItemId ItemLoc,
  roomLinks :: RoomLinkSet,

  eventQueue :: TimeQueue UTCTime Event
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
  eventQueue = TQ
    [(read "2012-08-19 16:10:00 UTC", TestEvent)]
}

$(deriveSafeCopy 0 'base ''World)

load :: IO (AcidState World)
load = openLocalState blankWorld

close :: AcidState World -> IO ()
close acid = closeAcidState acid


