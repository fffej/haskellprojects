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

-- |Builds a basic environment
createEnvironment :: Int -> Environment
createEnvironment s = Environment b s [(1,1),(s-1,s-1)] (mx,my)
    where
      (mx,my) = (s `div` 2, s `div` 2)
      b = M.fromList [((x,y),mkAgent x y) | x <- [0..s], y <- [0..s] ]
      mkAgent x y | x == 0 || y == 0 || x == s || y == s = AgentStack Obstacle []
                  | x == mx && y == my = AgentStack (Goal 1000) [Path 0]
                  | x == 1 && y == 1 = AgentStack Pursuer [Path 0]
                  | x == (s-1) && y == (s-1) = AgentStack Pursuer [Path 0]
                  | otherwise = AgentStack (Path 0) []

update :: Environment -> Environment
update e@(Environment b s _ _) = updatePursuers (e { board = c })
    where
      c = M.fromList [((x,y), diffusePoint' (x,y) c b) | y <- [0..s], x <- [0..s]]

-- TODO simplify
canMove :: Maybe AgentStack -> Bool
canMove Nothing = False
canMove (Just x) = case x of
                     AgentStack (Path _) _ -> True
                     _ -> False
                          
flipObstacle :: Point -> Environment -> Environment
flipObstacle p e | top x /= Obstacle  = e { board = M.insert p (push Obstacle x) b }
                 | null (rest x)      = e
                 | otherwise          = e { board = M.insert p (pop x) b }
    where
      b = board e
      x = b M.! p

flipPursuer :: Point -> Environment -> Environment
flipPursuer p e | top x /= Pursuer = e { board = M.insert p (push Pursuer x) b 
                                       , pursuers = p : pursuers e }
                | null (rest x)    = e
                | otherwise        = e { board = M.insert p (pop x) b
                                              , pursuers = delete p (pursuers e) }
    where
      b = board e
      x = b M.! p
      
                  

move :: Map Point AgentStack -> Point -> Point -> Map Point AgentStack
move e src tgt = M.insert src (pop srcA)
                 (M.insert tgt (push (top srcA) (e M.! tgt)) e) 
    where
      srcA = e M.! src

moveGoal :: Point -> Environment -> Environment
moveGoal p e | targetSuitable = e {
                                  board = move b (goal e) dest
                                , goal = dest
                                }
             | otherwise = e
    where
      b = board e
      dest = addPoint p (goal e)
      target = M.lookup dest b
      targetSuitable = canMove target

updatePursuers :: Environment -> Environment
updatePursuers env = foldl updatePursuer env (pursuers env)

updatePursuer :: Environment -> Point -> Environment
updatePursuer e p | null n = e
                  | otherwise = e { 
                                  board = move b p m 
                                , pursuers = m : delete p (pursuers e)
                                }
    where
      b = board e
      n = filter (canMove . (`M.lookup` b)) $ neighbouringPoints p -- TODO Further filtering
      m = maximumBy (\x y -> comparing (scent . top) (b M.! x) (b M.! y)) n

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
diffusePoint (AgentStack (Path d) r) n = AgentStack (Path $ diffusedScent d n) r
diffusePoint p _ = p

diffusedScent :: Scent -> [Agent] -> Scent
diffusedScent s xs = s + diffusionRate * sum (map (\x -> scent x - s) xs)
