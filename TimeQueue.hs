module TimeQueue where

import Control.Monad
import Data.Functor

import GameTime

data TimeQueue a = TQ [(GameTime,a)] deriving (Show)

empty :: TimeQueue a
empty = TQ []

getReadyEvents :: GameTime -> TimeQueue a -> ([a], TimeQueue a)
getReadyEvents t (TQ es) = (map snd l, TQ r) where
  (l, r) = span ((<= t) . fst) es

schedule :: GameTime -> a -> TimeQueue a -> TimeQueue a
schedule t' x (TQ es) = TQ es' where
  es' = l ++ (t',x) : r
  (l,r) = span ((<= t') . fst) es

debug :: (a -> String) -> TimeQueue a -> String
debug sh (TQ es) = unlines . map (\(t,x) -> show t ++ ": " ++ sh x) $ es


