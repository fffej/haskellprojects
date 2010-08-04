module Chase where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (mapMaybe,catMaybes)
import Data.List (maximumBy,delete)
import Data.Ord (comparing)

import Debug.Trace

-- Colloborate Diffusion
-- http://en.wikipedia.org/wiki/Antiobjects
type Desirability = Double
type Scent = Double
type Point = (Int,Int)

diffusionRate :: Double
diffusionRate = 0.4

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

scent :: Agent -> Scent
scent (Path s) = s
scent (Goal s) = s
scent _ = 0

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
update e@(Environment b s _ _) = traceShow (updatePursuers (e { board = c })) updatePursuers (e { board = c })
    where
      c = M.fromList [((x,y), diffusePoint' (x,y) c b) | y <- [0..s], x <- [0..s]]

canMove :: Maybe [Agent] -> Bool
canMove (Just (Path _:xs)) = True
canMove _ = False

flipAgent :: Agent -> Point -> Environment -> Environment
flipAgent a p e | head x /= a    = e { board = M.insert p (a:x) b }
                | null (tail x)  = e
                | otherwise      = e { board = M.insert p (tail x) b }
    where
      b = board e
      x = b M.! p

move :: Map Point [Agent] -> Point -> Point -> Map Point [Agent]
move e src tgt = M.insert src (tail srcA)
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
      targetSuitable = canMove (M.lookup dest b)

updatePursuers :: Environment -> Environment
updatePursuers env = foldl updatePursuer env (pursuers env)

updatePursuer :: Environment -> Point -> Environment
updatePursuer e p | null n = e
                  | otherwise = e { board = move b p m 
                                  , pursuers = m : delete p (pursuers e) }
    where
      b = board e
      n = filter (canMove . (`M.lookup` b)) $ neighbouringPoints p 
      m = maximumBy (\x y -> comparing (scent . head) (b M.! x) (b M.! y)) n

diffusePoint' :: Point -> Map Point [Agent] -> Map Point [Agent] -> [Agent]
diffusePoint' p xs og = diffusePoint (og M.! p) (neighbours' xs og p)

neighbouringPoints :: Point -> [Point]
neighbouringPoints p = map (addPoint p) [(-1,0), (0,-1), (1,0), (0, 1)]

neighbours' :: Map Point [Agent] -> Map Point [Agent] -> Point -> [Agent]
neighbours' xs m (x,y) = map head $ catMaybes [M.lookup (addPoint (x,y) (-1, 0 )) xs
                                              ,M.lookup (addPoint (x,y) (0 , -1)) xs
                                              ,M.lookup (addPoint (x,y) (1 , 0) ) m
                                              ,M.lookup (addPoint (x,y) (0 , 1) ) m]

neighbours :: Map Point [Agent] -> Point -> [Agent]
neighbours m p = map head $ mapMaybe (`M.lookup` m) (neighbouringPoints p)

diffusePoint :: [Agent] -> [Agent] -> [Agent]
diffusePoint (Path d:r) n = (Path $ diffusedScent d n) : r
diffusePoint p _ = p

diffusedScent :: Scent -> [Agent] -> Scent
diffusedScent s xs = s + diffusionRate * sum (map (\x -> scent x - s) xs)

-- test
test = Environment {board = M.fromList [((0,0),[Obstacle]),((0,1),[Obstacle]),((0,2),[Obstacle]),((0,3),[Obstacle]),((0,4),[Obstacle]),((0,5),[Obstacle]),((0,6),[Obstacle]),((0,7),[Obstacle]),((0,8),[Obstacle]),((0,9),[Obstacle]),((0,10),[Obstacle]),((0,11),[Obstacle]),((0,12),[Obstacle]),((1,0),[Obstacle]),((1,1),[]),((1,2),[Path 0.0,Path 0.0]),((1,3),[Path 0.0]),((1,4),[Path 0.0]),((1,5),[Path 0.0]),((1,6),[Path 0.0]),((1,7),[Path 0.0]),((1,8),[Path 0.0]),((1,9),[Path 0.0]),((1,10),[Path 0.0]),((1,11),[Path 0.0]),((1,12),[Obstacle]),((2,0),[Obstacle]),((2,1),[Path 0.0]),((2,2),[Path 0.0]),((2,3),[Path 0.0]),((2,4),[Path 0.0]),((2,5),[Obstacle,Path 0.0]),((2,6),[Pursuer,Path 0.0]),((2,7),[Pursuer,Path 0.0]),((2,8),[Obstacle,Path 0.0]),((2,9),[Path 0.0]),((2,10),[Path 0.0]),((2,11),[Path 0.0]),((2,12),[Obstacle]),((3,0),[Obstacle]),((3,1),[Path 0.0]),((3,2),[Path 0.0]),((3,3),[Path 0.0]),((3,4),[Path 0.0]),((3,5),[Obstacle,Path 0.0]),((3,6),[Obstacle,Path 0.0]),((3,7),[Obstacle,Path 0.0]),((3,8),[Obstacle,Path 0.0]),((3,9),[Path 0.0]),((3,10),[Path 0.0]),((3,11),[Path 0.0]),((3,12),[Obstacle]),((4,0),[Obstacle]),((4,1),[Path 0.0]),((4,2),[Path 0.0]),((4,3),[Path 0.0]),((4,4),[Path 0.0]),((4,5),[Obstacle,Path 0.0]),((4,6),[Path 0.0]),((4,7),[Path 0.0]),((4,8),[Obstacle,Path 0.0]),((4,9),[Path 0.0]),((4,10),[Path 0.0]),((4,11),[Path 0.0]),((4,12),[Obstacle]),((5,0),[Obstacle]),((5,1),[Path 0.0]),((5,2),[Path 0.0]),((5,3),[Path 0.0]),((5,4),[Path 0.0]),((5,5),[Path 0.0]),((5,6),[Path 0.0]),((5,7),[Path 0.0]),((5,8),[Path 0.0]),((5,9),[Path 0.0]),((5,10),[Path 0.0]),((5,11),[Path 0.0]),((5,12),[Obstacle]),((6,0),[Obstacle]),((6,1),[Path 0.0]),((6,2),[Path 0.0]),((6,3),[Path 0.0]),((6,4),[Path 0.0]),((6,5),[Path 0.0]),((6,6),[Path 0.0]),((6,7),[Path 0.0]),((6,8),[Path 0.0]),((6,9),[Path 0.0]),((6,10),[Path 0.0]),((6,11),[Path 0.0]),((6,12),[Obstacle]),((7,0),[Obstacle]),((7,1),[Path 0.0]),((7,2),[Path 0.0]),((7,3),[Path 0.0]),((7,4),[Path 0.0]),((7,5),[Path 0.0]),((7,6),[Path 0.0]),((7,7),[Path 0.0]),((7,8),[Path 0.0]),((7,9),[Path 0.0]),((7,10),[Path 0.0]),((7,11),[Path 0.0]),((7,12),[Obstacle]),((8,0),[Obstacle]),((8,1),[Path 0.0]),((8,2),[Path 0.0]),((8,3),[Path 0.0]),((8,4),[Path 0.0]),((8,5),[Obstacle,Path 0.0]),((8,6),[Path 0.0]),((8,7),[Path 0.0]),((8,8),[Path 0.0]),((8,9),[Path 0.0]),((8,10),[Path 0.0]),((8,11),[Path 0.0]),((8,12),[Obstacle]),((9,0),[Obstacle]),((9,1),[Path 0.0]),((9,2),[Path 0.0]),((9,3),[Path 0.0]),((9,4),[Path 0.0]),((9,5),[Path 0.0]),((9,6),[Path 0.0]),((9,7),[Path 0.0]),((9,8),[Path 0.0]),((9,9),[Path 0.0]),((9,10),[Path 0.0]),((9,11),[Path 0.0]),((9,12),[Obstacle]),((10,0),[Obstacle]),((10,1),[Path 0.0]),((10,2),[Path 0.0]),((10,3),[Path 0.0]),((10,4),[Obstacle,Path 0.0]),((10,5),[Obstacle,Path 0.0]),((10,6),[Path 400.0]),((10,7),[Path 160.0]),((10,8),[Path 64.0]),((10,9),[Path 25.6]),((10,10),[Path 10.240000000000002]),((10,11),[Path 4.096000000000001]),((10,12),[Obstacle]),((11,0),[Obstacle]),((11,1),[Path 0.0]),((11,2),[Path 0.0]),((11,3),[Path 0.0]),((11,4),[Path 0.0]),((11,5),[Path 400.0]),((11,6),[Goal 1000.0,Path 0.0]),((11,7),[Path 464.0]),((11,8),[Path 211.20000000000002]),((11,9),[Path 94.72000000000001]),((11,10),[Path 18.432000000000006,Path 41.98400000000001]),((11,11),[]),((11,12),[Obstacle]),((12,0),[Obstacle]),((12,1),[Obstacle]),((12,2),[Obstacle]),((12,3),[Obstacle]),((12,4),[Obstacle]),((12,5),[Obstacle]),((12,6),[Obstacle]),((12,7),[Obstacle]),((12,8),[Obstacle]),((12,9),[Obstacle]),((12,10),[Obstacle]),((12,11),[Obstacle]),((12,12),[Obstacle])], size = 12, pursuers = [(11,10),(1,2)], goal = (11,6)}