{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module TimeQueue where

import Control.Monad
import Control.Arrow
import Data.Functor
import Data.SafeCopy

import GameTime

data TimeQueue a = TQ [(GameTime,a)] deriving (Show)

$(deriveSafeCopy 0 'base ''TimeQueue)

empty :: TimeQueue a
empty = TQ []

getReadyEvents :: GameTime -> TimeQueue a -> ([a], TimeQueue a)
getReadyEvents t q = (map snd, TQ) $$ (splitAtTime t q)

schedule :: GameTime -> a -> TimeQueue a -> TimeQueue a
schedule t x q = TQ es' where
  es' = l ++ (t,x) : r
  (l,r) = splitAtTime t q

splitAtTime t (TQ es) = span ((<= t) . fst) es
($$) = uncurry (***)

debug :: (a -> String) -> TimeQueue a -> String
debug sh (TQ es) = unlines . map (\(t,x) -> show t ++ ": " ++ sh x) $ es


