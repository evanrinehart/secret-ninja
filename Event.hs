{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Event where

import Data.SafeCopy
import Data.Typeable

data Event =
  Checkpoint |
  Heal |
  Lightning |
  Spawn |
  GeneralUpdate deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Event)



