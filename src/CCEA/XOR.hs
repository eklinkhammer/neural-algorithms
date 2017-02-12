module CCEA.XOR
  (
    xorFitness
  , xorBreeding
  , xorEA
  , xorVars
  ) where

import NN.NeuralNetwork
import RandomUtil.Random
import AI.HNN.FF.Network

import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra.HMatrix
import Data.List (sortOn, partition, mapAccumL)
import System.Random
import System.Random.Shuffle
import Data.Tuple
import Data.Traversable

import CCEA.EA

xorVars :: NNVars
xorVars = nnVars

nnVars :: NNVars
nnVars = Map.fromList [("numberInputs", 2)
                      ,("numberHidden", 2)
                      ,("numberOutputs",1)
                      ,("timesToTrain",1)
                      ,("learningRate",0.25)
                      ,("sigmoidOrTanh",0)
                      ,("randomLowerBound", (-2))
                      ,("randomUpperBound", 2)
                      ,("gaussianMean", 0)
                      ,("gaussianStdDev", 1)
                      ,("mutationRate", 0.5)]

xorFitness :: NN n => FitnessFunction n
xorFitness = xorFitnessFunction nnVars

xorFitnessFunction :: NN n => NNVars -> n -> Double
xorFitnessFunction nnVars net = let x1  = (get nnVars net $ fromList [0.0,0.0]) ! 0
                                    x2  = (get nnVars net $ fromList [0.0,1.0]) ! 0
                                    x3  = (get nnVars net $ fromList [1.0,0.0]) ! 0
                                    x4  = (get nnVars net $ fromList [1.0,1.0]) ! 0
                                    x1e = 1 - abs (0 - x1)
                                    x2e = 1 - abs (1 - x2)
                                    x3e = 1 - abs (1 - x3)
                                    x4e = 1 - abs (0 - x4)
                                in (x1e + x2e + x3e + x4e) / 4

xorBreeding :: (RandomGen g, NN n) => BreedingStrategy n g
xorBreeding g = notElitist (xorBreedingStrategy nnVars) (xorBreedingStrategy nnVars) g

xorBreedingStrategy ::  (RandomGen g, NN n) => NNVars -> BreedingStrategy n g
xorBreedingStrategy = mutateWeights

xorEA :: RandomGen g => IO (EA (Network Double) g)
xorEA = do
  pop <- sequence $ map (\_ -> create nnVars) [1..100]
  return $ EA pop (tournament 2) xorFitness xorBreeding
