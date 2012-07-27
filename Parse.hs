{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8

data X = A | B | C deriving (Ord,Eq,Show)

basic :: Parser X
basic = do
  string "fuck"
  skipSpace
  arg <- takeByteString
  endOfInput
  case arg of
    "ok" -> return A
    "yeah" -> return B
    _ -> return C
