module Projects where

import Data.Time.Clock (NominalDiffTime)

data Project = Project
               {
                 name :: String
               , bestCaseEstimate :: NominalDiffTime
               , mostLikelyEstimate :: NominalDiffTime
               , worstCaseEstimate :: NominalDiffTime
               }  deriving (Show)

weightedAverage :: Project -> NominalDiffTime
weightedAverage p = (a + 4 * m + b) / 6
  where
    a = bestCaseEstimate p
    m = mostLikelyEstimate p
    b = worstCaseEstimate p

standardDeviation :: Project -> NominalDiffTime
standardDeviation p = (b - a) / 6
  where
    a = bestCaseEstimate p
    b = worstCaseEstimate p
