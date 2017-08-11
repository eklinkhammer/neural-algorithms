{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module NN.NeuralNetwork
  (
    NN (..)
  , NNVars
  , createNetworkW
  , Net (..)
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
  get :: n -> Vector Double -> Vector Double
  train :: n -> Sample Double -> n
  train net samp = trainV net [samp]
  trainV :: n -> Samples Double -> n
  randomize :: (RandomGen g) => g -> n -> (g,n)
  getWeights :: n -> V.Vector (Matrix Double)
  getVars :: n -> NNVars

data Net = Net
  {
    _network :: Network Double
  , _vars    :: NNVars
  }

instance NN Net where
  create vars = do
    n <- createNetworkW vars
    return $ Net n vars
  get net input = getNetwork (_vars net) (_network net) input
  trainV net io = let vars = _vars net in Net (trainNetwork vars (_network net) io) vars
  randomize g net = let vars = _vars net
                        (g',n) = randomizeNetwork vars g (_network net)
                    in (g', Net n vars)
  getWeights (Net (Network m) _) = m
  getVars (Net _ vars) = vars

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
randomizeNetworkReader g net@(Network weights) = do
  mean <- asks (getVar meanS)
  std  <- asks (getVar stddevS)
  rate <- asks (getVar mutationRate)
  let mutF = probApply rate (addNormalNoise mean std)
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
