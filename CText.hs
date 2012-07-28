{-# LANGUAGE OverloadedStrings #-}
module CText where

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

data CFrag = NCF Text | CF Color Text deriving (Eq,Ord,Show)
data CText = CText [CFrag] deriving (Eq,Ord)

instance Monoid CText where
  CText xs `mappend` CText ys = CText (xs ++ ys)
  mempty = CText []

instance IsString CText where
  fromString s = CText [NCF (fromString s)]

instance Show CText where
  show ct = (show . encode) ct

fromText :: Text -> CText
fromText txt = CText [NCF txt]

color :: Color -> Text -> CText
color c txt = CText [CF c txt]

codeTab :: Color -> Text
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

reset :: Text
reset = "\ESC[0m"

encodeFragment :: CFrag -> [Text]
encodeFragment (NCF txt) = [txt]
encodeFragment (CF c txt) = [codeTab c, txt, reset]

encode :: CText -> Text
encode (CText frags) = T.concat . concat . map encodeFragment $ frags
