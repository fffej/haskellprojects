module Chase where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (catMaybes)

-- Colloborate Diffusion
type Desirability = Double
type Scent = Double
type Point = (Int,Int)
type AgentStack = [Agent]

diffusionRate :: Double
diffusionRate = 0.25

data Agent = Goal Desirability
           | Pursuer Scent
           | Path Scent
           | Obstacle

data Environment = Environment {
      board :: Map Point AgentStack
}


diffuse :: Environment -> Environment
diffuse = undefined

neighbours :: Environment -> Point -> (Agent,Agent,Agent,Agent)
neighbours (Environment e) (x,y) = undefined
    where
      n = [ (x+dx,y+dy)  | (dx,dy) <- [(1,0),(-1,0),(1,0),(0,1)]] 
      c = catMaybes $ map ((flip M.lookup) e) n