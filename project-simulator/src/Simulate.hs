module Simulate where

import Projects

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time (UTCTime)

data Report = Report [ProjectCompletion] deriving (Show)

data ProjectCompletion = ProjectCompletion
                         {
                           project :: Project
                         , completionDates :: Map UTCTime Int
                         } deriving (Show)

simulate :: [Dependency] -> [Project] -> Report
simulate = undefined
