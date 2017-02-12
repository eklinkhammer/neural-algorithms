import Test.HUnit
import Matrix.StateTest
import RandomUtil.RandomMatrixTest
import NN.NeuralNetworkTest
import CCEA.EATest
import CCEA.XORTest

allTests :: Test
allTests = TestList [ TestLabel "Matrix.State Tests" testsMatrix
                    , TestLabel "RandomUtil.RandomMatrix Tests" testsRandomMatrix
                    , TestLabel "NN.NeuralNetwork Tests" testsNeuralNetwork
                    , TestLabel "CCEA.XOR Tests" xorTests
                    , TestLabel "CCEA.EA Tests" testsEA]
main :: IO Counts
main = runTestTT allTests
