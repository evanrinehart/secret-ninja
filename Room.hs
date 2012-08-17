{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Room where

import Data.SafeCopy

import IdWrappers

data Room = Room deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Room)

roomId0 :: RoomId
roomId0 = RoomId "58c497bc2b914cc03a82002b835b301bbf601601"

room0 :: Room
room0 = Room
