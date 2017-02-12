{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Matrix.Traversable
  (
    matrixTraverse
  ) where

import Numeric.LinearAlgebra.HMatrix hiding (corr)

import Data.Traversable

import Util.Tuples

-- Matrix cannot be an instance of Functor, so it cannot be an instance of traversable
-- instead, convert to list and use the list traverse
matrixTraverse :: (Element b, Element c) => (a -> b -> (a,c)) -> a -> Matrix b -> (a, Matrix c)
matrixTraverse f a m = applySnd (fromLists) $ mapAccumL (mapAccumL f) a (toLists m)
