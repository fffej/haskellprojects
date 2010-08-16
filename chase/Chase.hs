module Chase where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Array

import Data.Maybe (mapMaybe,catMaybes)
import Data.List (maximumBy,delete)
import Data.Ord (comparing)

import Debug.Trace

-- Colloborate Diffusion
-- http://en.wikipedia.org/wiki/Antiobjects
type Desirability = Double
type Scent = Double
type Point = (Int,Int)

data Agent = Goal Desirability
           | Pursuer 
           | Path Scent
           | Obstacle
             deriving (Eq,Show)

data Environment = Environment {
      board :: Map Point [Agent]
    , size :: Int
    , pursuers :: [Point]
    , goal :: Point
} deriving Show

diffusionRate :: Double
diffusionRate = 0.1

scent :: Agent -> Scent
scent (Path s) = s
scent (Goal s) = s
scent _ = 0

zeroScent :: Agent -> Agent
zeroScent (Path s) = Path 0
zeroScent x = x

zeroScents :: [Agent] -> [Agent]
zeroScents (x:xs) = zeroScent x : xs
zeroScents x = x

topScent :: [Agent] -> Scent
topScent (x:xs) = scent x
topScent _ = 0

addPoint :: Point -> Point -> Point
addPoint (x,y) (dx,dy) = (x+dx,y+dy)

-- |Builds a basic environment
createEnvironment :: Int -> Environment
createEnvironment s = Environment b s [(1,1),(s-1,s-1)] (mx,my)
    where
      (mx,my) = (s `div` 2, s `div` 2)
      b = M.fromList [((x,y),mkAgent x y) | x <- [0..s], y <- [0..s] ]
      mkAgent x y | x == 0 || y == 0 || x == s || y == s = [Obstacle]
                  | x == mx && y == my = [Goal 1000,Path 0]
                  | x == 1 && y == 1 = [Pursuer, Path 0]
                  | x == (s-1) && y == (s-1) = [Pursuer,Path 0]
                  | otherwise = [Path 0]

update :: Environment -> Environment
update e@(Environment b s _ _) = updatePursuers (e { board = c })
    where
      c = M.fromList [((x,y), diffusePoint' (x,y) c b) | y <- [0..s], x <- [0..s]]

-- TODO simplify?
canMove :: Maybe [Agent] -> Bool
canMove (Just (Path _:xs)) = True
canMove _ = False

flipObstacle :: Point -> Environment -> Environment
flipObstacle p e | head x /= Obstacle = e { board = M.insert p (Obstacle:x) b }
                 | null (tail x)      = e
                 | otherwise          = e { board = M.insert p (tail x) b }
    where
      b = board e
      x = b M.! p

-- |Hides the scent underneath
flipPursuer :: Point -> Environment -> Environment
flipPursuer p e | head x /= Pursuer = e { board = M.insert p (Pursuer:x) b 
                                       , pursuers = p : pursuers e }
                | null (tail x)    = e
                | otherwise        = e { board = M.insert p (tail x) b
                                       , pursuers = delete p (pursuers e) }
    where
      b = board e
      x = b M.! p
      
                  

move :: Map Point [Agent] -> Point -> Point -> Map Point [Agent]
move e src tgt = M.insert src (zeroScents $ tail srcA)
                 (M.insert tgt (head srcA : e M.! tgt) e) 
    where
      srcA = e M.! src

moveGoal :: Point -> Environment -> Environment
moveGoal p e | targetSuitable = e { board = move b (goal e) dest
                                  , goal = dest }
             | otherwise = e
    where
      b = board e
      dest = addPoint p (goal e)
      targetSuitable = canMove $ M.lookup dest b

updatePursuers :: Environment -> Environment
updatePursuers env = foldl updatePursuer env (pursuers env)

-- Ensure we only move if there is a better scent available
updatePursuer :: Environment -> Point -> Environment
updatePursuer e p | null n    = e
                  | otherwise = e { board = move b p m 
                                  , pursuers = m : delete p (pursuers e) }
    where
      b = board e
      currentScent = topScent (b M.! p)
      n = filter (\x -> topScent (b M.! x) >= currentScent ) $ 
          filter (canMove . (`M.lookup` b)) $ neighbouringPoints p  -- can simplify here
      m = maximumBy (\x y -> comparing (scent . head) (b M.! x) (b M.! y)) n

diffusePoint' :: Point -> Map Point [Agent] -> Map Point [Agent] -> [Agent]
diffusePoint' p xs originalGrid = diffusePoint (originalGrid M.! p) (neighbours' xs originalGrid p)

neighbouringPoints :: Point -> [Point]
neighbouringPoints p = map (addPoint p) [(-1,0), (0,-1), (1,0), (0, 1)]

neighbours' :: Map Point [Agent] -> Map Point [Agent] -> Point -> [Agent]
neighbours' xs m p = map head $ catMaybes [M.lookup (addPoint p (-1, 0 )) xs
                                          ,M.lookup (addPoint p (0 , -1)) xs
                                          ,M.lookup (addPoint p (1 , 0) ) m
                                          ,M.lookup (addPoint p (0 , 1) ) m]

neighbours :: Map Point [Agent] -> Point -> [Agent]
neighbours m p = map head $ mapMaybe (`M.lookup` m) (neighbouringPoints p)

diffusePoint :: [Agent] -> [Agent] -> [Agent]
diffusePoint (Path d:r) n = (Path $ diffusedScent d n) : r
diffusePoint p _ = p

diffusedScent :: Scent -> [Agent] -> Scent
diffusedScent s xs = s + diffusionRate * sum (map (\x -> scent x - s) xs)