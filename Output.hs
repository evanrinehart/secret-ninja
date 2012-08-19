{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Output where

import Data.ByteString
import qualified Data.ByteString.UTF8 as U
import Data.Text
import Data.Text.Encoding

class Output a where
  encode :: a -> ByteString

instance Output ByteString where
  encode = id

instance Output String where
  encode = U.fromString

instance Output Text where
  encode = encodeUtf8

