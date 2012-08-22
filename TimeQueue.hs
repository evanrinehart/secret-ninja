{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module TimeQueue where

import Control.Arrow
import Data.SafeCopy

data TimeQueue t a = TQ [(t,a)] deriving (Show)

$(deriveSafeCopy 0 'base ''TimeQueue)

empty :: TimeQueue t a
empty = TQ []

getReadyEvents :: Ord t => t -> TimeQueue t a -> ([(t,a)], TimeQueue t a)
getReadyEvents t q = (l, TQ r) where
  (l,r) = splitAtTime t q

schedule :: Ord t => t -> a -> TimeQueue t a -> TimeQueue t a
schedule t x q = TQ es' where
  es' = l ++ (t,x) : r
  (l,r) = splitAtTime t q

splitAtTime :: Ord t => t -> TimeQueue t a -> ([(t,a)], [(t,a)])
splitAtTime t (TQ es) = span ((<= t) . fst) es

shead :: TimeQueue t a -> Maybe (t,a)
shead (TQ []) = Nothing
shead (TQ (x:xs)) = Just x

nextTime :: TimeQueue t a -> Maybe t
nextTime q = shead q >>= \(t,_) -> Just t

debug :: Show t => (a -> String) -> TimeQueue t a -> String
debug sh (TQ es) = unlines . map (\(t,x) -> show t ++ ": " ++ sh x) $ es


