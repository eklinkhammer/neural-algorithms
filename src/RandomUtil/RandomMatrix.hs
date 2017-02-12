{-# LANGUAGE FlexibleContexts #-}

module RandomUtil.RandomMatrix
  (
    selectRandomMatrixElement
  , randomMatrixIndex
  , randomizeRandomMatrixElement
  , addGaussianNoiseToMatrixElement
  , addGaussianNoiseToRandomMatrixElement
  , randomNMatrixElements
  , RandomMatrixMutation
  ) where

import System.Random
import Data.Random.Normal
import Matrix.State
import Numeric.LinearAlgebra.HMatrix hiding (corr)

import Util.Tuples


type RandomMatrixMutation g a = g -> Matrix a -> (Matrix a, g)
type RandomMatrixIndex g = (MatrixIndex, g)

randomMatrixIndex :: RandomGen g => g -> Matrix a -> RandomMatrixIndex g
randomMatrixIndex gen m = let (i, gen')  = randomR (0, (rows m) - 1) gen
                              (j, gen'') = randomR (0, (cols m) - 1) gen'
                           in ((i,j), gen'')

selectRandomMatrixElement :: (Element a, Indexable (Vector a) a, RandomGen g)
  => g -> Matrix a -> (a, g)
selectRandomMatrixElement gen m = let (i, gen') = randomMatrixIndex gen m
                                  in (getElement m i, gen')                                    

randomizeRandomMatrixElement :: (Element a, Random a, RandomGen g)
  =>  (a,a) -> RandomMatrixMutation g a
randomizeRandomMatrixElement bounds gen m = let (i, gen')  = randomMatrixIndex gen m
                                                (e, gen'') = randomR bounds gen'
                                            in (replaceMatrixElement i e m, gen'')

type Mean = Double
type Sigma = Double
addGaussianNoiseToRandomMatrixElement :: RandomGen g =>  Mean -> Sigma -> RandomMatrixMutation g Double
addGaussianNoiseToRandomMatrixElement u s gen m =
  let (i,gen') = randomMatrixIndex gen m
  in addGaussianNoiseToMatrixElement i u s gen' m

addGaussianNoiseToMatrixElement :: RandomGen g
  => MatrixIndex -> Mean -> Sigma -> RandomMatrixMutation g Double
addGaussianNoiseToMatrixElement index u s gen m =
  let (sample, gen') = normal' (u,s) gen
  in (applyFMatrixElement (+ sample) index m, gen')

randomNMatrixElements :: (Element a, Random a, RandomGen g)
  => Int -> RandomMatrixMutation g a -> RandomMatrixMutation g a
randomNMatrixElements 0 _ gen m = (m, gen)
randomNMatrixElements n f gen m = let (m', gen') = f gen m
                                  in randomNMatrixElements (n-1) f gen' m'

