{-# LANGUAGE OverloadedStrings #-}
module Misc where

import Data.ByteString.Char8
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent.MVar
import Data.String


crlf :: IsString a => a
crlf = "\r\n"

