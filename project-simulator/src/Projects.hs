module Projects where

-- TODO smart constructor to hide the use of double internally
-- and only allow construction with a nominal diff time

data Project = Project
               {
                 name :: String
               , bestCaseEstimate :: Double
               , mostLikelyEstimate :: Double
               , worstCaseEstimate :: Double
               }  deriving (Show)

weightedAverage :: Project -> Double
weightedAverage p = (a + 4 * m + b) / 6
  where
    a = bestCaseEstimate p
    m = mostLikelyEstimate p
    b = worstCaseEstimate p

standardDeviation :: Project -> Double
standardDeviation p = (b - a) / 6
  where
    a = bestCaseEstimate p
    b = worstCaseEstimate p

