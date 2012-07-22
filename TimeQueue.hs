{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module TimeQueue where

import Control.Monad
import Control.Arrow
import Data.Functor
import Data.SafeCopy
import Data.Time

data TimeQueue t a = TQ [(t,a)] deriving (Show)

$(deriveSafeCopy 0 'base ''TimeQueue)

empty :: TimeQueue t a
empty = TQ []

getReadyEvents :: Ord t => t -> TimeQueue t a -> ([a], TimeQueue t a)
getReadyEvents t q = (map snd, TQ) $$ (splitAtTime t q)

schedule :: Ord t => t -> a -> TimeQueue t a -> TimeQueue t a
schedule t x q = TQ es' where
  es' = l ++ (t,x) : r
  (l,r) = splitAtTime t q

splitAtTime t (TQ es) = span ((<= t) . fst) es
($$) = uncurry (***)

debug :: Show t => (a -> String) -> TimeQueue t a -> String
debug sh (TQ es) = unlines . map (\(t,x) -> show t ++ ": " ++ sh x) $ es


