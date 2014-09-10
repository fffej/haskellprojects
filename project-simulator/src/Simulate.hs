module Simulate where

import Projects

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time (UTCTime)

import Data.Random
import Data.Random.Distribution.Triangular

data Report = Report [ProjectCompletion] deriving (Show)

data ProjectCompletion = ProjectCompletion
                         {
                           project :: Project
                         , completionDates :: Map UTCTime Int
                         } deriving (Show)

simulate :: [Dependency] -> [Project] -> Report
simulate = undefined

estimate :: Project -> [ProjectCompletion]
estimate p = undefined


normalPair :: RVar (Double,Double)
normalPair =  do
     u <- stdUniform
     t <- stdUniform
     let r = sqrt (-2 * log u)
         theta = (2 * pi) * t
         
         x = r * cos theta
         y = r * sin theta
     return (x,y)

