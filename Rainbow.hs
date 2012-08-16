{-# LANGUAGE OverloadedStrings #-}
module Rainbow where

{-
public api here...
data Color
data Rainbow a (Monoid, IsString)
color :: Color -> a -> Rainbow a
compile :: (IsString a, Monoid a) => Rainbow a -> a
-}


import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.String

import Output

data Color =
  Black |
  Red |
  Green |
  Yellow |
  Blue |
  Magenta |
  Cyan |
  White |
  BrightBlack |
  BrightRed |
  BrightGreen |
  BrightYellow |
  BrightBlue |
  BrightMagenta |
  BrightCyan |
  BrightWhite deriving (Eq,Ord,Show)

data CFrag a = NCF a | CF Color a deriving (Eq,Ord,Show)
data Rainbow a = Rainbow (Endo [CFrag a])

instance Monoid (Rainbow a) where
  Rainbow f `mappend` Rainbow g = Rainbow (f `mappend` g)
  mempty = Rainbow mempty

instance IsString a => IsString (Rainbow a) where
  fromString = fragment . NCF . fromString

instance Output a => Show (Rainbow a) where
  show = show . encode

instance Output a => Output (Rainbow a) where
  encode = compile encode

fragment :: CFrag a -> Rainbow a
fragment cf = (Rainbow . Endo) ([cf] ++)

color :: Color -> a -> Rainbow a
color c txt = fragment (CF c txt)

codeTab :: Color -> ByteString
codeTab Black = "\ESC[30m"
codeTab Red = "\ESC[31m"
codeTab Green = "\ESC[32m"
codeTab Yellow = "\ESC[33m"
codeTab Blue = "\ESC[34m"
codeTab Magenta = "\ESC[35m"
codeTab Cyan = "\ESC[36m"
codeTab White = "\ESC[37m"
codeTab BrightBlack = "\ESC[1m\ESC[30m"
codeTab BrightRed = "\ESC[1m\ESC[31m"
codeTab BrightGreen = "\ESC[1m\ESC[32m"
codeTab BrightYellow = "\ESC[1m\ESC[33m"
codeTab BrightBlue = "\ESC[1m\ESC[34m"
codeTab BrightMagenta = "\ESC[1m\ESC[35m"
codeTab BrightCyan = "\ESC[1m\ESC[36m"
codeTab BrightWhite = "\ESC[1m\ESC[37m"

reset :: ByteString
reset = "\ESC[0m"

compile :: (a -> ByteString) -> Rainbow a -> ByteString
compile encode (Rainbow (Endo f)) = r where
  r = BS.concat bss
  bss = concat $ map g (f [])
  g (NCF a) = [encode a]
  g (CF c a) = [codeTab c, encode a, reset]

