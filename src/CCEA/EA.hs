{-# OPTIONS -Wall #-}

module CCEA.EA
  (
    EA (..)
  , evolve
  , evolveN
  , tournament
  , truncation
  , bestHalf
  , elitist
  , notElitist
  , fitnessProp
  , mutateWeights
  , SelectionStrategy
  , FitnessFunction
  , BreedingStrategy
  ) where

import Util.Tuples
import System.Random.Shuffle
import System.Random
import Data.List
import Util.MyList
import RandomUtil.Random
import NN.NeuralNetwork

type Population i = [i]
-- EA ::= Evolutionary algorithm
-- An evolutionary algorithm is parametarized by the type of its individuals and the type of
--  its fitness.
-- Each EA ha the population it works with, as well as the selection strategy, fitness function,
--  and breeding strategy
data EA i g = EA (Population i) (SelectionStrategy i g) (FitnessFunction i) (BreedingStrategy i g)
  
-- A selection strategy is a function that takes a list of individuals with a
-- score and outputs the selected surviving individuals
-- The output list will be half (integer division) the size of the input list
type SelectionStrategy i g = (g -> [(i,Double)] -> (g,[i]))

-- A fitness function (for an evolutionary algorithm) maps an individual to
-- its fitness value. 
type FitnessFunction i = (i -> Double)

-- A Breeding strategy is a function for transforming a population into a mutated
-- one. It is assumed that the next generation will be twice the size of the
-- parents. However, the elitist and nonElitist functions double the size. A user
-- defined breeding strategy should maintain list size
type BreedingStrategy i g = (g -> [i] -> (g,[i]))

-- There are three steps to evolutionary algorithms
-- 1) select best fit individuals for reproduction
-- 2) Breed new individuals
-- 3) Evaluate fitness of new individuals
-- 4) Replace least fit individuals

evolveN :: RandomGen g => Int -> g -> EA i g -> (g, EA i g)
evolveN 0 g ea = (g,ea)
evolveN n g ea = let (g', evolved) = evolve g ea
                 in evolveN (n - 1) g' evolved

evolve :: g -> EA i g -> (g, EA i g)
evolve g (EA pop sel fit breed) =
  let (g', offspring) = breed g pop
      withFit         = zip offspring $ map fit offspring
      (g'',selected)  = sel g' withFit
  in (g'', EA selected sel fit breed)

fitnessProp :: RandomGen g => SelectionStrategy i g
fitnessProp g popWithFit = (g,map fst selected)
  where
    totalFitness = foldl (\a (_,b) -> a + b) 0.0 popWithFit :: Double
    popWithNormFit = map (applySnd (\x -> x / totalFitness)) popWithFit
    sortedPop = sortOn snd popWithNormFit
    rs = randomStream g
    is = filter ((/=) (-1)) $ map (toIndex sortedPop 0) rs
    -- have a list of indices
    -- now, remove duplicates and take length over 2
    -- LAZY???
    selected = selectIndices sortedPop $ sort (take (length popWithFit `div` 2) (nub is))
    

toIndex :: [(i,Double)] -> Int -> Double -> Int
toIndex [] _ _ = (-1)
toIndex ((_,d):xs) n d' = if d' > d then n else toIndex xs (n+1) d'

tournament :: RandomGen g => Int -> SelectionStrategy i g
tournament _ g [] = (g,[])
tournament subgroup g popWithFit = let shuffled = shuffle' popWithFit (length popWithFit) g
                                       winner   = selectBest (take subgroup shuffled)
                                   in applySnd (\x -> winner : x) (tournament subgroup g (drop subgroup shuffled))

selectBest :: [(i,Double)] -> i
selectBest = fst . last . sortOn snd


truncation :: Int -> SelectionStrategy i g
truncation n g xs = (g, take n $ map fst $ reverse $ sortOn snd xs)

bestHalf :: SelectionStrategy i g
bestHalf g xs = truncation (length xs `div` 2) g xs

elitist :: BreedingStrategy i g -> BreedingStrategy i g
elitist strat g parents = let (g', offspring) = strat g parents
                          in (g', offspring ++ parents)

notElitist :: BreedingStrategy i g -> BreedingStrategy i g -> BreedingStrategy i g
notElitist s1 s2 g parents = let (g', offspring1) = s1 g parents
                                 (g'', offspring2) = s2 g' parents
                             in (g'', offspring1 ++ offspring2)

mutateWeights :: (RandomGen g, NN n) => BreedingStrategy n g
mutateWeights g nets = mapAccumL randomize g nets
