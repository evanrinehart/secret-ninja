{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Output where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text
import Data.Text.Encoding

class Output a where
  encode :: a -> ByteString

instance Output ByteString where
  encode = id

instance Output String where
  encode = C8.pack

instance Output Text where
  encode = encodeUtf8

