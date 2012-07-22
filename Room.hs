{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Room where

import Data.SafeCopy

data Room = Room deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Room)
