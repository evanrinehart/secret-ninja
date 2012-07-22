{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Item where

import Data.SafeCopy

data Item = Item deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Item)
