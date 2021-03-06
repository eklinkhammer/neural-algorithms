module NN.NeuralNetworkTest
  (
    testsNeuralNetwork
  ) where

import Test.HUnit
import AI.HNN.FF.Network
import Numeric.LinearAlgebra.HMatrix hiding (corr)

import NUtil
import NN.NeuralNetwork
import qualified Data.Map.Strict as Map

nnVars :: Vars
nnVars = Map.fromList [("numberInputs", 2)
                      ,("numberHidden", 2)
                      ,("numberOutputs", 1)
                      ,("timesToTrain", 3)
                      ,("learningRate", 0.8)
                      ,("sigmoidOrTanh", 0)]


samples :: Samples Double
samples = [ fromList [0, 0] --> fromList [0]
          , fromList [0, 1] --> fromList [1]
          , fromList [1, 0] --> fromList [1]
          , fromList [1, 1] --> fromList [0] 
          ]

expectedOutputVals :: [Double]
expectedOutputVals = [0,1,1,0]
          
testsNeuralNetwork :: Test
testsNeuralNetwork = TestList [ TestLabel "label" testsLabel]

testsLabel :: Test
testsLabel = TestList [ TestLabel "test1" testCreate
                      , TestLabel "test2" testGet
                      , TestLabel "test3" testTrainV]

testCreate :: Test
testCreate = TestCase (do
                          net <- create nnVars :: IO Net
                          let smartNet = trainNTimes 1500 0.8 tanh tanh' (_network net) samples
--                              outputValues = map (get smartNet . fst) samples
                              outputValues = convert1 $ map (output smartNet tanh . fst) samples
                          assertEqual "testCreate"
                            True
                            (floatVectorCompare outputValues expectedOutputVals 0.2))

testGet :: Test
testGet = TestCase (do
                       net <- create nnVars :: IO Net
                       let smartNet = trainNTimes 1000 0.8 tanh tanh' (_network net) samples
                           outputValues = convert1 $ map (get (Net smartNet nnVars) . fst) samples
                       assertEqual "testGet"
                         True
                         (floatVectorCompare outputValues expectedOutputVals 0.2))

testTrainV :: Test
testTrainV = TestCase (do
                         net <- create nnVars :: IO Net
                         let smartNet = repeatNet trainV net samples 1000
                             outputValues = convert1 $ map (get smartNet . fst) samples
                         assertEqual "testTrainV"
                           True
                           (floatVectorCompare outputValues expectedOutputVals 0.2))

                         
convert1 :: [Vector Double] -> [Double]
convert1 = map (! 0)

floatVectorCompare :: [Double] -> [Double] -> Double -> Bool
floatVectorCompare a b tol = foldr (&&) True $ zipWith (\x y -> abs (x - y) < tol ) a b

repeatNet :: (n -> c -> n) -> n -> c -> Int -> n
repeatNet _ net _ 0 = net
repeatNet f net c n = let net' = f net c in repeatNet f net' c (n-1)
