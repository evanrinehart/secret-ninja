{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Item where

import Data.SafeCopy
import Data.Typeable

data Item = Item deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''Item)
