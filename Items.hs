{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Items where

import Data.SafeCopy
import Data.Typeable

import IdWrappers

data ItemLoc =
  InRoom RoomId |
  InMob MobId |
  InItem ItemId deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''ItemLoc)
