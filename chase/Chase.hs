module Chase where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (catMaybes)

import Debug.Trace

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
             deriving (Show)

-- |An attempt to enforce that this can't be empty
data AgentStack = AgentStack {
      top :: Agent
    , rest :: [Agent]
} deriving Show

data Environment = Environment {
      board :: Map Point AgentStack
    , size :: Int
} deriving Show

scent :: Agent -> Scent
scent (Pursuer s) = s
scent (Path s) = s
scent (Goal s) = s
scent _ = 0

createEnvironment size = Environment b size
    where
      b = M.fromList [((x,y),mkAgent x y) | x <- [0..size], y <- [0..size] ]
      mkAgent x y | x == 0 || y == 0 || x == size || y == size = AgentStack Obstacle []
                  | x == (size `div` 2) && y == (size `div` 2) = AgentStack (Goal 1000) []
                  | x == 1 && y == 1 = AgentStack (Pursuer 0) []
                  | x == (size - 1) && y == (size - 1) = AgentStack (Pursuer 0) []
                  | otherwise = AgentStack (Path 0) []

update :: Environment -> Environment
update e@(Environment b size) = e { board = c }
    where
      c = M.fromList [((x,y), diffusePoint' (x,y) c b) | y <- [0..size], x <- [0..size]]

diffusePoint' :: Point -> Map Point AgentStack -> Map Point AgentStack -> AgentStack
diffusePoint' p xs originalGrid = diffusePoint (originalGrid M.! p) (neighbours xs originalGrid p)

diffusePoint :: AgentStack -> [Agent] -> AgentStack
diffusePoint (AgentStack (Goal d) r) n = AgentStack (Goal d) r
diffusePoint (AgentStack Obstacle r) n = AgentStack Obstacle r
diffusePoint (AgentStack (Path d) r) n = AgentStack (Path $ diffusedScent d n) r -- TODO
diffusePoint (AgentStack (Pursuer d) r) n = AgentStack (Pursuer $ diffusedScent d n) r

diffusedScent :: Scent -> [Agent] -> Scent
diffusedScent s xs = s + diffusionRate * sum (map (\x -> scent x - s) xs)

-- So I can lazily build a list to do Map.fromList
-- TODO I want to lazily build a map?  This is SO INEFFICIENT...
neighbours :: Map Point AgentStack -> Map Point AgentStack -> Point -> [Agent]
neighbours xs m (x,y) = map top $ catMaybes [M.lookup (x-1,y) xs
                                            ,M.lookup (x,y-1) xs
                                            ,M.lookup (x+1,y) m
                                            ,M.lookup (x,y+1) m]
