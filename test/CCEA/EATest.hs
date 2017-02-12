{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CCEA.EATest
  (
    testsEA
  ) where

import Test.HUnit

import NN.NeuralNetwork
import CCEA.EA
import CCEA.XOR

import System.Random
import Numeric.LinearAlgebra.HMatrix hiding (corr)
import AI.HNN.FF.Network

import Data.List (sortOn)
         
-- Testing EA requires a selection strategy, a fitness function, and a breeding strategy

test1 :: Test
test1 = TestCase (assertEqual "string" True True)


testsEA :: Test
testsEA = TestList [TestLabel "test1" test1]
