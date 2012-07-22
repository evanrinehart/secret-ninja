module Rng where

import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.ByteString as B

type Rng = Gen (PrimState IO)

newRng :: IO Rng
newRng = do
--  h <- fOpen "/dev/urandom"
--  raw <- B.hGet 16000 h
--  seed = f raw
--  initialize seed
  create

take :: Variate a => Rng -> IO a
take g = uniform g

takeR :: Variate a => (a,a) -> Rng -> IO a
takeR r g = uniformR r g


