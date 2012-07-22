{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Mob where

import Data.SafeCopy

data Mob = Mob deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Mob)
