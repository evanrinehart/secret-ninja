{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Item where

import Data.SafeCopy
import Data.Typeable

import IdWrappers

data Item = Item deriving (Eq,Ord,Show,Typeable)
data ItemLoc =
  InRoom RoomId |
  InMob MobId |
  InItem ItemId deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''ItemLoc)
$(deriveSafeCopy 0 'base ''Item)

itemId01 = ItemId "8239ab32b8fd059a0780654d4db1b5502e0b246c"
itemId02 = ItemId "58c497bc2b914cc03a82002b835b301bbf601601"
itemId03 = ItemId "63fb49e7168682034ff087d0d6b9e1ae6cdd09b0"

