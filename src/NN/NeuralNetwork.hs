{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module NN.NeuralNetwork
  (
    NN (..)
  , NNVars
  , createNetworkW
  , Network (..)
  , module Util.Vars
  ) where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra.HMatrix hiding (corr)
import System.Random (RandomGen, random)
import qualified Data.Vector as V
import Control.Monad.Reader
import RandomUtil.Random
import Util.Vars

import Matrix.Traversable
import Data.Traversable

type NNVars = Vars

class NN n where
  create :: NNVars -> IO n
  get :: NNVars -> n -> Vector Double -> Vector Double
  train :: NNVars -> n -> Sample Double -> n
  train vars net samp = trainV vars net [samp]
  trainV :: NNVars -> n -> Samples Double -> n
  randomize :: (RandomGen g) => NNVars -> g -> n -> (g,n)
  getWeights :: n -> V.Vector (Matrix Double)

instance NN (Network Double) where
  create vars                   = createNetworkW vars
  get vars net input            = getNetwork vars net input
  trainV vars net io            = trainNetwork vars net io 
  randomize vars g net          = randomizeNetwork vars g net
  getWeights (Network m)        = m

createNetworkW :: NNVars -> IO (Network Double)
createNetworkW env = runReader createNetworkReader env

createNetworkReader :: Reader NNVars (IO (Network Double))
createNetworkReader = do
  numInput      <- asks (getVar numberInputsS)
  numHidden     <- asks (getVar numberHiddenS)
  numOutput     <- asks (getVar numberOutputsS)
  return $ createNetwork (round numInput) [(round numHidden)] (round numOutput)

getNetwork :: NNVars -> Network Double -> Vector Double -> Vector Double
getNetwork vars net input = runReader (getNetworkReader net input) vars

getNetworkReader :: Network Double -> Vector Double -> Reader NNVars (Vector Double)
getNetworkReader net input = do
  sigOrTanh <- asks (getVar sigmoidTanS)
  let (act, _) = getActivationFunctions sigOrTanh
  return $ output net act input

trainNetwork :: NNVars -> Network Double -> Samples Double -> Network Double
trainNetwork vars net samples = runReader (trainNetworkReader net samples) vars

trainNetworkReader :: Network Double -> Samples Double -> Reader NNVars (Network Double)
trainNetworkReader net samples = do
  timesToTrain  <- asks (getVar timesToTrainS)
  learningRate  <- asks (getVar learningRateS)
  sigOrTanh     <- asks (getVar sigmoidTanS)
  let (act, act') = getActivationFunctions sigOrTanh
  return $ trainNTimes (round timesToTrain) learningRate act act' net samples

type RandNet g = g -> Network Double -> (g, Network Double)

randomizeNetwork :: RandomGen g => NNVars -> RandNet g
randomizeNetwork vars g net = runReader (randomizeNetworkReader g net) vars

randomizeNetworkReader :: RandomGen g
  => g -> Network Double -> Reader NNVars (g, Network Double)
randomizeNetworkReader g net = do
  mean <- asks (getVar meanS)
  std  <- asks (getVar stddevS)
  rate <- asks (getVar mutationRate)
  let mutF = probApply rate (addNormalNoise mean std)
      weights = getWeights net
      (g', weights') = mapAccumL (matrixTraverse mutF) g weights 
  return $ (g',fromWeightMatrices weights')


type MutateElementF g a b = (g -> a -> (g, b))

-- With a probability of the provided double, apply the random function
-- Otherwise, apply nothing
probApply :: RandomGen g
  => Double -> MutateElementF g a a -> MutateElementF g a a
probApply p rF g a = let (r,g') = random g
                     in if r > p then randomID g' a else rF g' a

randomID :: MutateElementF g a a
randomID g a = (g,a)
                       
getActivationFunctions :: (Floating a) => Double -> (ActivationFunction a, ActivationFunctionDerivative a)
getActivationFunctions inp = let asInt :: Integer
                                 asInt = round inp
                             in case asInt of
                                  1 -> (sigmoid, sigmoid')
                                  _ -> (tanh, tanh')
