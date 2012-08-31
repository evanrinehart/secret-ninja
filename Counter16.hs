{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Counter16 where

import Data.Typeable
import Control.Applicative
import Data.Serialize
import System.Random
import Data.Bits
import Data.List
import Data.Word
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.SafeCopy

newtype Counter16 = Counter16 Integer deriving (Show, Read, Typeable)

instance Serialize Counter16 where
  put (Counter16 n) = (put . BS.pack . explode 16) n
  get = (Counter16 . implode . BS.unpack) <$> getBytes 16

instance Random Counter16 where
  random g = (Counter16 x, g') where
    (x, g') = randomR (0, bigN - 1) g
  randomR (Counter16 a, Counter16 b) g = (Counter16 x, g') where
    (x, g') = randomR (a,b) g


cmap :: (Integer -> Integer) -> Counter16 -> Counter16
cmap f (Counter16 n) = Counter16 ((f n) `mod` bigN)

plusOne :: Counter16 -> Counter16
plusOne = cmap (+1)

hash :: Counter16 -> ByteString
hash (Counter16 n) = SHA256.hash . BS.pack . explode 16 $ n

zero :: Counter16
zero = Counter16 0

---

bigN = 2 ^ 128

explode :: Int -> Integer -> [Word8]
explode l =
  take l .
  map fromIntegral .
  unfoldr (\x -> Just (x .&. 255, x `shiftR` 8))

implode :: [Word8] -> Integer
implode ws =
  foldr (.|.) 0 $
  zipWith shiftL (map fromIntegral ws) (map (*8) [0..])

$(deriveSafeCopy 0 'base ''Counter16)
