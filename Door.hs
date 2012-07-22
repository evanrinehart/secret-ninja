{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Door where

import Data.SafeCopy

data Door = Door deriving (Eq,Ord,Show)

$(deriveSafeCopy 0 'base ''Door)
