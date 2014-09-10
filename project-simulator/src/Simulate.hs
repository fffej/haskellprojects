{-# LANGUAGE MultiParamTypeClasses #-}

module Simulate where

import Projects

import Data.Random
import Data.Random.Distribution.Triangular
import Control.Monad

data Report = Report [ProjectCompletion] deriving (Show)

data ProjectCompletion = ProjectCompletion
                         {
                           project :: Project
                         , completionTimes :: [Double]
                         } deriving (Show)

sampleSize :: Int
sampleSize = 100000

simulate :: [Project] -> Report
simulate = undefined

estimate :: MonadRandom m => Project -> m [Double]
estimate p = replicateM sampleSize (sample $ pdf p)

pdf :: Project -> RVar Double
pdf p = floatingTriangular
          (bestCaseEstimate p)
          (mostLikelyEstimate p)
          (worstCaseEstimate p)

normalPair :: RVar (Double,Double)
normalPair =  do
     u <- stdUniform
     t <- stdUniform
     let r = sqrt (-2 * log u)
         theta = (2 * pi) * t
         
         x = r * cos theta
         y = r * sin theta
     return (x,y)

