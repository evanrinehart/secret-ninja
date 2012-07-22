{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module RoomLink where

import Data.IxSet
import Control.Applicative
import Data.Typeable
import Data.SafeCopy

import IdWrappers
import Names
import Door

newtype FromRoom = FromRoom RoomId deriving (Eq,Ord,Show,Typeable)
newtype ToRoom = ToRoom RoomId deriving (Eq,Ord,Show,Typeable)
$(deriveSafeCopy 0 'base ''FromRoom)
$(deriveSafeCopy 0 'base ''ToRoom)

data RoomLink = RoomLink {
  id :: RoomLinkId,
  name :: Names,
  fromRoom :: FromRoom,
  toRoom :: ToRoom,
  door :: Maybe Door
} deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''RoomLink)

type RoomLinkSet = IxSet RoomLink

instance Indexable RoomLink where
  empty = ixSet [
      ixFun (pure . RoomLink.id),
      ixFun (pure . fromRoom),
      ixFun (pure . toRoom)
    ]

from :: RoomId -> RoomLinkSet -> [RoomLink]
from rid = toList . getEQ (FromRoom rid)

to :: RoomId -> RoomLinkSet -> [RoomLink]
to rid = toList . getEQ (ToRoom rid)

find :: RoomLinkId -> RoomLinkSet -> Maybe RoomLink
find rlid = getOne . getEQ rlid

delete :: RoomLinkId -> RoomLinkSet -> RoomLinkSet
delete rlid = deleteIx rlid

write :: RoomLink -> RoomLinkSet -> RoomLinkSet
write rl = updateIx (RoomLink.id rl) rl

modify :: RoomLinkId -> (RoomLink -> RoomLink) -> RoomLinkSet -> RoomLinkSet
modify rlid f s = maybe s (flip write s . f) (find rlid s)

