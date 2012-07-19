{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module WorldState0 where

{-
the world is stored in an acid state
queries and updates must happen in IO
-}

import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader

import YMap

type WorldState = WorldState0

data WorldState0 = WorldState0 {
  dummy0 :: String,
  dummy1 :: YMap Integer String,
  dummy2 :: [Bool]
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''WorldState0)

writeState :: WorldState -> Update WorldState ()
writeState w = put w

writeY :: YMap Integer String -> Update WorldState ()
writeY s = do
  w <- get
  put (w {dummy1 = s})

queryState :: Query WorldState WorldState
queryState = ask

queryY :: Query WorldState (YMap Integer String)
queryY = do
  w <- ask
  return (dummy1 w)

$(makeAcidic ''WorldState0
  ['writeState, 'queryState, 'queryY, 'writeY])
{-
queryState0 :: Query WorldState0 WorldState0
queryState0 = ask

$(makeAcidic ''WorldState0 ['queryState0])
-}
