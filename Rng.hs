{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Rng where

import Crypto.Hash.SHA256
import System.Random
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Data.Bits
import Data.List
import System.IO
import Data.Typeable
import Data.SafeCopy

newtype Rng = Rng Integer deriving (Show, Read, Typeable)

$(deriveSafeCopy 0 'base ''Rng)

instance RandomGen Rng where
  next (Rng n) = (extract4 n, Rng (succ128 n))
  split g = let (x, g') = randomR (0,2^128-1) g in (g', Rng x)
  genRange _ =
    (fromIntegral (minBound :: Int32)
    ,fromIntegral (maxBound :: Int32))

extract4 :: Integer -> Int
extract4 = extractN 4

new :: IO Rng
new = do
  h <- openFile "/dev/urandom" ReadMode
  seed <- BS.hGet h 16
  hClose h
  return (restore seed)

explode :: Int -> Integer -> [Word8]
explode l =
  take l .
  map fromIntegral .
  unfoldr (\x -> Just (x .&. 255, x `shiftR` 8))

implode :: [Word8] -> Integer
implode ws =
  foldr (.|.) 0 $
  zipWith shiftL (map fromIntegral ws) (map (*8) [0..])

save :: Rng -> ByteString
save (Rng n) = (BS.pack . explode 16) n

restore :: ByteString -> Rng
restore = Rng . implode . BS.unpack . BS.take 16

succ128 :: Integer -> Integer
succ128 n = (n+1) `mod` (2^128)

extractN :: Num a => Int -> Integer -> a
extractN n =
  fromIntegral .
  implode .
  BS.unpack .
  BS.take n .
  hash .
  BS.pack .
  explode 16

extract :: Rng -> (ByteString, Rng)
extract (Rng n) = ((hash . BS.pack . explode 16) n, (Rng . succ128) n)

-- the randomPick is broken because Integer has no Variate instance
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
