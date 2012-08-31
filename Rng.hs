{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Rng where

import System.Random
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO
import Data.Typeable
import Data.SafeCopy
import Data.Int
import Data.Serialize

import Counter16

{-
this rng uses a 16 byte counter

to extract random data from it, the counter is hashed using a
crypto hash function and then incremented (next)

to seed the generator, use 16 bytes of random data (restore)
-}

type Rng = Counter16

instance RandomGen Counter16 where
  next g = (extract4 . hash $ g, plusOne g)
  split g = random g
  genRange _ =
    (fromIntegral (minBound :: Int32)
    ,fromIntegral (maxBound :: Int32))

new :: IO Rng
new = do
  h <- openFile "/dev/urandom" ReadMode
  seed <- BS.hGet h 16
  hClose h
  return (restore seed)

save :: Rng -> ByteString
save = encode

restore :: ByteString -> Rng
restore = either error id . decode

---

extract4 :: ByteString -> Int
extract4 = extractN 4

extractN :: Num a => Int -> ByteString -> a
extractN n =
  fromIntegral .
  implode .
  BS.unpack .
  BS.take n

-- need to rewrite in terms of new interface
{-
randomPick :: [a] -> Rng -> IO a
randomPick [] g = error "picking from empty list"
randomPick (x:xs) g = randomPick' xs g 2 x

randomPick' :: [a] -> Rng -> Integer -> a -> IO a
randomPick' [] g n y = return y
randomPick' (x:xs) g n y = oneOutOf n g a b where
  a = randomPick' xs g (n+1) x
  b = randomPick' xs g (n+1) y

oneOutOf :: Integer -> Rng -> IO a -> IO a -> IO a
oneOutOf n g io0 io1 = do
  m <- randomR (1,fromIntegral n :: Int) g --totally wrong
  if m == 1 then io0 else io1
-}

