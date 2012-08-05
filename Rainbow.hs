{-# LANGUAGE OverloadedStrings #-}
module Rainbow where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.String

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

instance (IsString a, Show a, Monoid a) => Show (Rainbow a) where
  show = show . compile

fragment :: CFrag a -> Rainbow a
fragment cf = (Rainbow . Endo) ([cf] ++)

color :: Color -> a -> Rainbow a
color c txt = fragment (CF c txt)

codeTab :: IsString a => Color -> a
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

reset :: IsString a => a
reset = "\ESC[0m"

encodeFragment :: IsString a => CFrag a -> [a]
encodeFragment (NCF a) = [a]
encodeFragment (CF c a) = [codeTab c, a, reset]

compile :: (IsString a, Monoid a) => Rainbow a -> a
compile (Rainbow (Endo f)) = (mconcat . concat . map encodeFragment) (f [])


