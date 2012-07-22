{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module IdWrappers where

import Data.Typeable
import Data.SafeCopy
import Data.ByteString

type RawId = ByteString

newtype RoomId = RoomId RawId deriving (Eq,Ord,Show,Typeable)
newtype RoomLinkId = RoomLinkid RawId deriving (Eq,Ord,Show,Typeable)
newtype MobId = MobId RawId deriving (Eq,Ord,Show,Typeable)
newtype ItemId = ItemId RawId deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''RoomId)
$(deriveSafeCopy 0 'base ''RoomLinkId)
$(deriveSafeCopy 0 'base ''MobId)
$(deriveSafeCopy 0 'base ''ItemId)
