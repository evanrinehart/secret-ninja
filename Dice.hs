module Dice where

import Control.Monad.State
import Data.Acid
import System.Random

import Rng
import WorldType
import IdWrappers

withRng :: State Rng a -> Update World a
withRng s = do
  MainRng g <- gets rng
  let (x, g') = runState s g
  modify (\w -> w {rng = MainRng g'})
  return x

withAuxRng :: State Rng a -> Update World a
withAuxRng s = do
  AuxRng g <- gets idGen
  let (x, g') = runState s g
  modify (\w -> w {idGen = AuxRng g'})
  return x

-- roll nd6
roll :: Int -> Update World [Int]
roll n = withRng $ replicateM n (state $ randomR (1,6))

randomId :: Update World RawId
randomId = withAuxRng $ do
  ns <- replicateM 40 (state $ randomR (0,15))
  return (numbersToId ns)

-- 5d6 IO
roll' :: IO ([Int], Int)
roll' = do
  ds <- replicateM 5 $ randomRIO (1,6)
  return (ds, sum (filter (>= 5) ds))
