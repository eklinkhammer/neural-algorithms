module NN.EvolveNet
  (

  ) where

-- Network without backprop. Used for fast genetic algorithms
-- Borrowed partially from Gatlin (github)

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Algorithms.Matrix (mmultS, transpose2S)
import Data.Array.Repa.Algorithms.Randomish (randomishDoubleArray)
import System.Random
import RandomUtil.Random
import Data.Traversable

type Matrix a     = Array a DIM2
type Weights      = Matrix U Double
type Activation a = (a -> a)
data EvolveNet    = Net
  {
    _weights :: [Weights]
  , _act     :: (Activation Double)
  }

randomWeights :: Int -> Int -> Weights
randomWeights r c = randomishDoubleArray (Z :. r :. c) (-1) 1 100

randomNetwork :: Int -> [Int] -> Int -> Activation Double -> EvolveNet
randomNetwork i hs o f =
  let weights = randomAllWeights ([i] ++ hs ++ [o])
  in Net weights f

randomAllWeights :: [Int] -> [Weights]
randomAllWeights (p:x:xs) = (randomWeights x (p+1)) : randomAllWeights (x:xs)
randomAllWeights _       = []

netOutput :: Matrix U Double -> EvolveNet -> Matrix U Double
netOutput inp net = foldl (feedForward (_act net)) inp (_weights net)

feedForward :: (Double -> Double) -> Matrix U Double -> Weights -> Matrix U Double
feedForward f input weightMatrix = R.computeS $ R.map f $ mmultS weightMatrix (addDefaultNode input)

addDefaultNode :: Matrix U Double -> Matrix U Double
addDefaultNode mat = let n = R.fromListUnboxed (Z :. 1 :. 1) [1]
                     in R.computeS $ R.append mat n

probApply :: RandomGen g => Double -> (g -> a -> (g,a)) -> g -> a -> (g,a)
probApply p f g a = let (r,g') = random g in if r > p then (g',a) else f g' a

mutateWeights :: RandomGen g => Double -> g -> Weights -> (g, Matrix D Double)
mutateWeights p g w = let (g', r) = mapAccumL (probApply p (addNormalNoise 0 1)) g w
                      in (g', R.computeS r)
