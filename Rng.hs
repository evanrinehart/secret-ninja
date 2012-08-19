module Rng where

import System.Random.MWC
import Control.Monad.Primitive

type Rng = Gen (PrimState IO)

newRng :: IO Rng
newRng = create -- need to seed this with /dev/urandom

random :: Variate a => Rng -> IO a
random g = uniform g

randomR :: Variate a => (a,a) -> Rng -> IO a
randomR r g = uniformR r g

-- the randomPick is broken because Integer has no Variate instance
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

