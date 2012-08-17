{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.Text
import qualified Data.Text as T
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding

data TestCommand = List | End deriving (Show)

telnetPassword :: Parser ByteString
telnetPassword = do
  hmm <- peekWord8
  case hmm of
    Nothing -> return ""
    Just w -> if w == 255
      then fmap (BS.drop 3) takeByteString
      else takeByteString

testCommand :: Parser TestCommand
testCommand = do
  skipSpace
  w <- choice [string "list", string "quit"]
  skipSpace
  endOfInput
  return $ case w of
    "list" -> List
    "quit" -> End
    _ -> undefined
