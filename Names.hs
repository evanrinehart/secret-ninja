{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Names where

import Data.SafeCopy
import Data.Typeable
import Data.Text
import qualified Data.List as L

data Names = Names {
  primary :: Text,
  alts :: [Text]
} deriving (Eq,Ord,Typeable)

instance Show Names where
  show (Names x _) = show x

$(deriveSafeCopy 0 'base ''Names)

all :: Names -> [Text]
all (Names x xs) = x:xs

elem :: Text -> Names -> Bool
elem t (Names x xs) = t == x || t `L.elem` xs


