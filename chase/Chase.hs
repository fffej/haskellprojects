module Chase where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (mapMaybe,catMaybes,fromJust)
import Data.List (maximumBy,delete)
import Data.Ord (comparing)

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
             deriving (Eq,Show)

-- |An attempt to enforce that this can't be empty
data AgentStack = AgentStack {
      top :: Agent
    , rest :: [Agent]
} deriving Show

data Environment = Environment {
      board :: Map Point AgentStack
    , size :: Int
    , pursuers :: [Point]
    , goal :: Point
} deriving Show

scent :: Agent -> Scent
scent (Pursuer s) = s
scent (Path s) = s
scent (Goal s) = s
scent _ = 0

addPoint :: Point -> Point -> Point
addPoint (x,y) (dx,dy) = (x+dx,y+dy)

pop :: AgentStack -> AgentStack
pop (AgentStack _ (x:xs)) = AgentStack x xs
pop x@(AgentStack _ _)    =  error ("Cannot pop an empty stack " ++ show x)

push :: Agent -> AgentStack -> AgentStack
push a (AgentStack t r) = AgentStack a (t:r)

createEnvironment :: Int -> Environment
createEnvironment s = Environment b s [(1,1),(s-1,s-1)] (s `div` 2, s `div` 2)
    where
      b = M.fromList [((x,y),mkAgent x y) | x <- [0..s], y <- [0..s] ]
      mkAgent x y | x == 0 || y == 0 || x == s || y == s = AgentStack Obstacle []
                  | x == (s `div` 2) && y == (s `div` 2) = AgentStack (Goal 1000) [Path 0]
                  | x == 1 && y == 1 = AgentStack (Pursuer 0) [Path 0]
                  | x == (s-1) && y == (s-1) = AgentStack (Pursuer 0) [Path 0]
                  | otherwise = AgentStack (Path 0) []

update :: Environment -> Environment
update e@(Environment b s _ _) = updatePursuers (e { board = c })
    where
      c = M.fromList [((x,y), diffusePoint' (x,y) c b) | y <- [0..s], x <- [0..s]]

moveGoal :: Point -> Environment -> Environment
moveGoal p e | targetSuitable = e {
                                  board = M.insert (goal e) (pop src) 
                                          (M.insert dest (push (top src) (fromJust target)) b)
                                , goal = dest
                                }
             | otherwise = e
    where
      b = board e
      dest = addPoint p (goal e)
      src = b M.! (goal e)
      target = M.lookup dest b
      targetSuitable = maybe False (\x -> top x /= Obstacle) target -- TODO move to top level

updatePursuers :: Environment -> Environment
updatePursuers env = foldl updatePursuer env (pursuers env)

updatePursuer :: Environment -> Point -> Environment
updatePursuer e p = e { 
                      board = M.insert p source (M.insert m target b) 
                    , pursuers = m : delete p (pursuers e)
                    }
    where
      b = board e
      n = filter (`M.member` b) $ neighbouringPoints p
      m = maximumBy (\x y -> comparing (scent . top) (b M.! x) (b M.! y)) n
      source = pop (b M.! p) 
      target = push (top (b M.! p)) (b M.! m)

diffusePoint' :: Point -> Map Point AgentStack -> Map Point AgentStack -> AgentStack
diffusePoint' p xs originalGrid = diffusePoint (originalGrid M.! p) (neighbours' xs originalGrid p)

neighbouringPoints :: Point -> [Point]
neighbouringPoints p = map (addPoint p) [(-1,0), (0,-1), (1,0), (0, 1)]

neighbours' :: Map Point AgentStack -> Map Point AgentStack -> Point -> [Agent]
neighbours' xs m (x,y) = map top $ catMaybes [M.lookup (addPoint (x,y) (-1, 0 )) xs
                                             ,M.lookup (addPoint (x,y) (0 , -1)) xs
                                             ,M.lookup (addPoint (x,y) (1 , 0) ) m
                                             ,M.lookup (addPoint (x,y) (0 , 1) ) m]

neighbours :: Map Point AgentStack -> Point -> [Agent]
neighbours m p = map top $ mapMaybe (`M.lookup` m) (neighbouringPoints p)

diffusePoint :: AgentStack -> [Agent] -> AgentStack
diffusePoint (AgentStack (Goal d) r) _ = AgentStack (Goal d) r
diffusePoint (AgentStack Obstacle r) _ = AgentStack Obstacle r
diffusePoint (AgentStack (Path d) r) n = AgentStack (Path $ diffusedScent d n) r
diffusePoint (AgentStack (Pursuer d) r) n = AgentStack (Pursuer $ diffusedScent d n) r

diffusedScent :: Scent -> [Agent] -> Scent
diffusedScent s xs = s + diffusionRate * sum (map (\x -> scent x - s) xs)
