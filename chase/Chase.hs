module Chase where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (mapMaybe)

-- Colloborate Diffusion
-- http://en.wikipedia.org/wiki/Antiobjects
type Desirability = Double
type Scent = Double
type Point = (Int,Int)

diffusionRate :: Double
diffusionRate = 0.25

data Agent = Goal Desirability
           | Pursuer Scent
           | Path Scent
           | Obstacle

-- |An attempt to enforce that this can't be empty
data AgentStack = AgentStack {
      top :: Agent
    , rest :: [Agent]
}

data Environment = Environment {
      board :: Map Point AgentStack
    , size :: Int
}

createEnvironment size = Environment b size
    where
      b = M.fromList [((x,y),mkAgent x y) | x <- [0..size], y <- [0..size] ]
      mkAgent x y | x == 0 || y == 0 || x == size || y == size = AgentStack Obstacle []
                  | x == (size `div` 2) && y == (size `div` 2) = AgentStack (Goal 100) []
                  | x == 1 && y == 1 = AgentStack (Pursuer 0) []
                  | x == (size - 1) && y == (size - 1) = AgentStack (Pursuer 0) []
                  | otherwise = AgentStack (Path 0) []


diffuse :: Environment -> Environment
diffuse = undefined

neighbours :: Environment -> Point -> [Agent]
neighbours (Environment e _) (x,y) = map top $ mapMaybe (`M.lookup` e) n
    where
      n = [ (x+dx,y+dy) | (dx,dy) <- [(1,0),(-1,0),(1,0),(0,1)]] 
