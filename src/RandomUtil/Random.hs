{-# OPTIONS -Wall #-}


module RandomUtil.Random
  (
    randomMap
  , randomMapTwoGens
  , randomBool
  , bernoulliBools
  , selectNRankProb
  , randomStream
  , addNormalNoise
  ) where

import System.Random
import Data.Random.Normal

import Data.Traversable


-- Basically just mapAccumL flipped. Thank you stack overflow for the help
randomMap :: (g -> a -> (b,g)) -> g -> [a] -> ([b],g)
randomMap _ g []     = ([], g)
randomMap f g (x:xs) = let (y,g') = f g x
                           (ys, gf) = randomMap f g' xs
                       in (y:ys, gf)

randomMapTwoGens :: (g -> g -> a -> (b,(g,g))) -> g -> g -> [a] -> ([b], (g,g))
randomMapTwoGens _ g1 g2 []     = ([], (g1, g2))
randomMapTwoGens f g1 g2 (x:xs) = let (y, (g1', g2'))  = f g1 g2 x
                                      (ys, gs) = randomMapTwoGens f g1' g2' xs
                                  in (y:ys, gs)

randomBool :: RandomGen g => g -> Double -> (g, Bool)
randomBool g percent = (g', bool)
  where
    (val,g') = random g
    bool     = val < percent
    
-- Creates an infinite list of booleans that have percent chance of being true
bernoulliBools :: RandomGen g => g -> Double -> (g,[Bool])
bernoulliBools g percent = randomBools g (repeat percent)

randomBools :: RandomGen g => g -> [Double] -> (g,[Bool])
randomBools = mapAccumL randomBool

rankProbs :: Int -> [Double]
rankProbs len = probs
  where
    total = n * (n + 1) / 2
    n = fromIntegral len :: Double
    range = [n, (n - 1)..1]
    probs = map (/ total) range

rankProbsAccum :: Int -> [Double]
rankProbsAccum = tail . scanl (+) 0.0 . rankProbs

randomStream :: RandomGen g => g -> [Double]
randomStream g = snd $ mapAccumL (\gl _ -> let (x,g') = random gl
                                           in (g',x)) g ([1..] :: [Integer])

type Prob = Double
type Probs = [Prob]

-- Assuming that the list of objects is sorted in rank order, selects with rank
--  probabiltiy an element from that list and returns the element and the list with
--  said element removed
selectRankProb :: RandomGen g => g -> [a] -> ((a,[a]),g)
selectRankProb g xs = ((selected, rest), g')
  where
    accumProbs = rankProbsAccum (length xs)
    index = indexProb r accumProbs
    (r, g') = random g
    (before, selAndAfter) = splitAt index xs
    selected = head selAndAfter
    rest = before ++ tail selAndAfter

selectNRankProb :: RandomGen g => Int -> g -> [a] -> ([a],g)
selectNRankProb 0 g _ = ([],g)
selectNRankProb n g xs = let ((sel, rest), g') = selectRankProb g xs
                             (selected,g'')    = selectNRankProb (n - 1) g' rest
                         in (sel : selected, g'')

indexProb :: Prob -> Probs -> Int
indexProb prob = length . takeWhile (< prob)

type Mean = Double
type Sigma = Double

addNormalNoise :: RandomGen g => Mean -> Sigma -> g -> Double -> (g, Double)
addNormalNoise u s g x = let (sample, g') = normal' (u,s) g
                         in (g', x + sample)

