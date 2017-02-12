{-# OPTIONS -Wall #-}

module CCEA.CCEA
  (
    evolveCCEA
  , evolveNCCEA
  , CCEA (..)
  , liftStateless
  , createPopulation
  , CCEAVars
  , Population
  ) where

import System.Random

import Data.List

import Util.Vars
import NN.NeuralNetwork

import CCEA.EA

type CCEAVars = Vars

-- limitation - no breeding between pools
data CCEA n s g = CCEA { _pop   :: Population n
                         , _fit   :: CCEAFitnessFunction n s
                         , _breed :: BreedingStrategy n g
                         , _sel   :: SelectionStrategy n g
                         , _state :: s}

-- Evaluates the fitness of a team, carrying some state forward
type CCEAFitnessFunction n s = (s -> [n] -> (s, [Double]))

evolveNCCEA :: RandomGen g => Int -> g -> CCEA n s g -> (g, CCEA n s g)
evolveNCCEA 0 g ccea = (g,ccea)
evolveNCCEA n g ccea = let (g', evolved) = evolveCCEA g ccea
                       in evolveNCCEA (n - 1) g' evolved
                          
evolveCCEA :: g -> CCEA n s g -> (g, CCEA n s g)
evolveCCEA g (CCEA p ff bf sf st) = (g'', CCEA selected ff bf sf state)
  where
    (g', offspringPop) = mapAccumL bf g p
    teams              = transpose offspringPop
    (state, scores)    = mapAccumL ff st teams
    teamsWithScores    = zipWith zip teams scores
    popWithScores      = transpose teamsWithScores
    (g'', selected)    = mapAccumL sf g' popWithScores


liftStateless :: FitnessFunction n -> CCEAFitnessFunction n ()
liftStateless f _ xs = ((), map f xs)

type Population n = [[n]]
  
createPool :: NN n => Int -> NNVars -> IO [n]
createPool poolSize vars = sequence $ map (\_ -> create vars) [1..poolSize]

createPopulation :: NN n => Int -> Int -> NNVars -> IO (Population n)
createPopulation numPools poolSize vars = sequence $ map (\_ -> createPool poolSize vars) [1..numPools]
