module CCEA.XORTest
  (
    xorTests
  ) where

import Test.HUnit

import CCEA

import NN.NeuralNetwork

import Data.List
import System.Random

xorTests :: Test
xorTests = TestList [TestLabel "XORTestEA" xorTest1
                    ,TestLabel "XORTestCCEA" xorTest2]

xorTest1 :: Test
xorTest1 = TestCase (do
                       ea <- xorEA
                       g  <- getStdGen
                       let (EA finalPop _ _ _) = snd $ evolveN 100 g ea
                           trainedBestNet = bestNetwork finalPop
                           finalScore = xorFitness trainedBestNet
                       assertEqual "test1" True (fuzzyEqual finalScore 1.0))

xorTest2 :: Test
xorTest2 = TestCase (do
                        pop <- createPopulation 5 100 xorVars :: IO (Population (Network Double))
                        g   <- getStdGen
                        let ccea = CCEA pop (liftStateless xorFitness) xorBreeding (tournament 2) ()
                            (_,cceaF) = evolveNCCEA 100 g ccea
                            bestFinalNet = bestNetwork (map bestNetwork (_pop cceaF))
                            finalScore = xorFitness bestFinalNet
                        assertEqual "test2" True (fuzzyEqual finalScore 1.0))

fuzzyEqual :: Double -> Double -> Bool
fuzzyEqual x y = abs (x - y) < 0.1

bestNetwork :: NN n => [n] -> n
bestNetwork nets = let popWithFit = zip nets $ map xorFitness nets
                   in fst $ head $ reverse $ sortOn snd popWithFit
