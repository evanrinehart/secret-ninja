{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.Text
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding

data TestCommand =
  Blank |
  List |
  End |
  StopServer |
  Gossip Text deriving (Show)

word :: ByteString -> TestCommand -> Parser TestCommand
word str com = do
  skipSpace
  string str
  skipSpace
  endOfInput
  return com

wordRest :: ByteString -> (Text -> TestCommand) -> Parser TestCommand
wordRest str f = do
  skipSpace
  string str
  space
  skipSpace
  fmap (f . decodeUtf8) takeByteString

blank :: Parser TestCommand
blank = do
  skipSpace
  endOfInput
  return Blank

testCommand :: Parser TestCommand
testCommand = choice
  [blank
  ,word "list" List
  ,word "quit" End
  ,word "shutdown" StopServer
  ,wordRest "shout" Gossip]


