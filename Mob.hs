{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Mob where

import Data.SafeCopy

import IdWrappers

data Mob = Mob deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Mob)


mobId0 :: MobId
mobId0 = MobId "8239ab32b8fd059a0780654d4db1b5502e0b246c"

mob0 :: Mob
mob0 = Mob
