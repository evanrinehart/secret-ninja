{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Names where

import Data.SafeCopy
import Data.Typeable
import qualified Data.List as L

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as C8
import Data.Attoparsec.Char8

data Names a = Names {
  primary :: a,
  alts :: [a]
} deriving (Eq,Ord,Typeable,Show)

$(deriveSafeCopy 0 'base ''Names)

all :: Names a -> [a]
all (Names x xs) = x:xs

elem :: Eq a => a -> Names a -> Bool
elem t (Names x xs) = t == x || t `L.elem` xs


