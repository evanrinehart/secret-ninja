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
