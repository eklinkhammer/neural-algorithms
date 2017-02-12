module RandomUtil.RandomMatrixTest
  (
    testsRandomMatrix
  ) where

import Test.HUnit

import Numeric.LinearAlgebra.HMatrix hiding (corr)
import System.Random

import RandomUtil.RandomMatrix
import Matrix.State

mat :: Matrix Double
mat = reshape 3 $ fromList $ take 9 [1..]

data Two = Two

instance RandomGen Two where
  next g  = (2, g)
  split g = (g, g)

testsRandomMatrix :: Test
testsRandomMatrix = TestList [ TestLabel "selectRandomMatrixElement" testsSelectRandomMatrixElement,
                               TestLabel "randomMatrixIndex" testsRandomMatrixIndex
                             ]


testsSelectRandomMatrixElement :: Test
testsSelectRandomMatrixElement = TestList [TestLabel "test1" testSelects]
testsRandomMatrixIndex :: Test
testsRandomMatrixIndex = TestList [TestLabel "test1" testInBounds]

-- testsSelectRandomMatrixElement
testSelects :: Test
testSelects = TestCase (assertEqual "selectRandomMatrixElement - Select"
                         5
                         (fst $ selectRandomMatrixElement Two mat))

-- testsRandomMatrixIndex
testInBounds :: Test
testInBounds = TestCase (do gen <- getStdGen
                            let listI  = getListRandomIndex gen 100 mat
                            let result = checkAllTrue listI (isValid mat)
                            assertEqual "randomMatrixIndex - Indices in bounds" True result)

              
-- testsRandomMatrixIndex helper function
isValid :: Matrix a -> MatrixIndex -> Bool
isValid m (i,j) = i >= 0 && j >= 0 && i < rows m && j < cols m

getListRandomIndex :: (Element a) => StdGen -> Int -> Matrix a -> [MatrixIndex]
getListRandomIndex _ 0 _ = []
getListRandomIndex g n m = rIndex : getListRandomIndex g' (n-1) m
  where
    (rIndex,g') = randomMatrixIndex g m

checkAllTrue :: [a] -> (a -> Bool) -> Bool
checkAllTrue xs f = foldr (&&) True $ map f xs
