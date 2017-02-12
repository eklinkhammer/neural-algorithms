module Util.Vars
  (
    Vars
  , getVar
  , sigmoidTanS
  , numberInputsS
  , numberHiddenS
  , numberOutputsS
  , timesToTrainS
  , learningRateS 
  , mutationStratS
  , meanS
  , stddevS
  , randomUpperBoundS
  , randomLowerBoundS
  , mutationRate
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

getVar :: String -> Vars -> Double
getVar name bindings = fromJust (Map.lookup name bindings)

type Vars = Map.Map String Double

-- Strings used to get Reader Vars
sigmoidTanS :: String
sigmoidTanS = "sigmoidOrTanh"
numberInputsS :: String
numberInputsS = "numberInputs"
numberHiddenS :: String
numberHiddenS = "numberHidden"
numberOutputsS :: String
numberOutputsS = "numberOutputs"
timesToTrainS :: String
timesToTrainS = "timesToTrain"
learningRateS :: String
learningRateS = "learningRate"
mutationStratS :: String -- 1 for gaussian noise, 2 for random within range
mutationStratS = "mutationStrat"
meanS :: String
meanS = "gaussianMean"
stddevS :: String
stddevS = "gaussianStdDev"
randomUpperBoundS :: String
randomUpperBoundS = "randomUpperBound"
randomLowerBoundS :: String
randomLowerBoundS = "randomLowerBound"
numberOutputToRandomizeS :: String
numberOutputToRandomizeS = "numberOutputToRandomize"
numberHiddenToRandomizeS :: String
numberHiddenToRandomizeS = "numberHiddenToRandomize"
mutationRate :: String
mutationRate = "mutationRate"
