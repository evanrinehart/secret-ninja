module TimeQueue where

import Control.Monad
import Data.Functor

import GameTime

data TimeQueue a = TQ [(GameTime,a)] deriving (Show)

empty :: TimeQueue a
empty = TQ []

getReadyEvents :: GameTime -> TimeQueue a -> ([a], TimeQueue a)
getReadyEvents t q = (map snd l, TQ r) where
  (l,r) = splitAtTime t q

schedule :: GameTime -> a -> TimeQueue a -> TimeQueue a
schedule t x q = TQ es' where
  es' = l ++ (t,x) : r
  (l,r) = splitAtTime t q

splitAtTime t (TQ es) = span ((<= t) . fst) es

debug :: (a -> String) -> TimeQueue a -> String
debug sh (TQ es) = unlines . map (\(t,x) -> show t ++ ": " ++ sh x) $ es


