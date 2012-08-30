{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module IdWrappers where

import Control.Monad
import Data.Typeable
import Data.SafeCopy
import Data.Char
import qualified Data.ByteString.Char8 as C8
import Data.String

import Rng

newtype RawId = RawId C8.ByteString deriving (Eq,Ord,Typeable)

instance Show RawId where
  show (RawId bs) = C8.unpack bs

instance IsString RawId where
  fromString = RawId . C8.pack

newtype RoomId = RoomId RawId deriving (Eq,Ord,Show,Typeable)
newtype RoomLinkId = RoomLinkid RawId deriving (Eq,Ord,Show,Typeable)
newtype MobId = MobId RawId deriving (Eq,Ord,Show,Typeable)
newtype ItemId = ItemId RawId deriving (Eq,Ord,Show,Typeable)

$(deriveSafeCopy 0 'base ''RawId)
$(deriveSafeCopy 0 'base ''RoomId)
$(deriveSafeCopy 0 'base ''RoomLinkId)
$(deriveSafeCopy 0 'base ''MobId)
$(deriveSafeCopy 0 'base ''ItemId)

numbersToId :: [Int] -> RawId
numbersToId = RawId . C8.pack . map intToDigit

