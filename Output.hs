{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Output where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Text
import Data.Text.Encoding

class Output a where
  encode :: a -> ByteString

instance Output ByteString where
  encode = id

instance Output String where
  encode = BSU.fromString

instance Output Text where
  encode = encodeUtf8

