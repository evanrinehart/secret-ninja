module WorldState where

import TimeQueue

type Key = ByteString
newtype MobId = MobId Key
newtype RoomId = RoomId Key
newtype ItemId = ItemId Key
data ItemLoc =
  InRoom RoomId | InMob MobId | InItem ItemId deriving (Ord,Typeable)

newtype FromRoom = FromRoom RoomId
newtype ToRoom = ToRoom RoomId

data Door = Door {
  open :: Bool,
  lock :: Lock,
  desc :: Text
}

data RoomLink = RoomLink {
  names :: [Text],
  from :: RoomId,
  to :: RoomId,
  door :: Maybe Door
}

data WorldState = WorldState {
  mobLocations :: YMap MobId RoomId,
  itemLocations :: YMap ItemId ItemLoc,
  roomLinks :: YMap RoomId RoomLink,

  rooms :: Map RoomId Room,
  mobs :: Map MobId Mob,
  items :: Map ItemId Item,

  oldTime :: UTCTime,
  eventQueue :: TimeQueue Event
} deriving (Show)



data Event = Lightning | NoEffect | WakeMob MobId | Heal


updateGame :: RWST Config WorldState W IO ()
updateGame = do
  old <- gets oldTime
  now <- liftIO getCurrentTime
  nextEvent


executeNewEvents :: GameTime -> State WorldState ()
executeNewEvents t = do
  events <- dequeueNewEvents t
  forM_ events $ \e -> case e of
    Lightning -> globalMsg
    Heal -> everyones health + 1
    NoEffect -> return ()
    WakeMob mid -> 
  

dequeueNewEvents :: GameTime -> State WorldState [Event]
dequeueNewEvents t = do
  let (events, eq') = getReadyEvents (gets eventQueue)
  modify (\w -> w {eventQueue = eq'})
  return events




-- player input handler setup...
Player -> Text -> WorldState -> (Text, WorldState)
Text -> Command
Command -> WorldState -> 
