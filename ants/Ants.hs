module Ants where

import Control.Monad
import Control.Concurrent.STM

import Data.Ord (comparing)
import Data.Maybe
import Data.Array
import Data.List (sortBy)
import Data.Map (Map,unionWith)
import qualified Data.Map as M

import System.Random

-- |Dimensions of square world
dim :: Int
dim = 80

-- |Number of ants
nantsSqrt :: Int
nantsSqrt = 7

-- |Number of places with food
foodPlaces :: Int
foodPlaces = 35

-- |Range of amount of food at a place
foodRange :: Int
foodRange = 100

-- |Evaporation rate
evapRate :: Double
evapRate = 0.99

homeOff :: Int
homeOff = dim `div` 4

type TCell = TVar Cell

type TCellArray = Array (Int,Int) TCell

data World = World {
      cells :: TCellArray
} 

data Direction = N | NE | E | SE | S | SW | W | NW
               deriving (Enum,Show,Eq)

turnRight :: Direction -> Direction
turnRight NW = N
turnRight x = succ x

turnLeft :: Direction -> Direction
turnLeft N = NW
turnLeft x = pred x

turnInt :: Direction -> Int -> Direction
turnInt d 0 = d
turnInt d x = turnInt (turnRight d) (x - 1)

turnAround :: Direction -> Direction
turnAround = turnRight . turnRight . turnRight . turnRight

data Ant = Ant {
      direction :: Direction
    , hasFood :: Bool
} deriving (Eq,Show)
 
data Cell = Cell {
      food :: Int
    , pheromone :: Double
    , ant :: Maybe Ant
    , home :: Bool
} deriving (Eq,Show)

instance Ord Cell where
    compare = comparing food

data Agent = Agent Ant

{- Boring helper functions -}

clearAnt :: Cell -> Cell
clearAnt cell = cell { ant = Nothing }

incPher :: Cell -> Cell
incPher cell = cell { pheromone = succ (pheromone cell) }

setFood :: Cell -> Int -> Cell
setFood cell f = cell { food = f }

hasAnt :: Cell -> Bool
hasAnt (Cell _ _ (Just _) _) = True
hasAnt _ = False

homeRange :: [Int]
homeRange = [homeOff..(nantsSqrt + homeOff)]

delta :: Direction -> (Int,Int)
delta N  = (0,-1)
delta NE = (1,-1)
delta E  = (1,0)
delta SE = (1,1)
delta S  = (0,1)
delta SW = (-1,1)
delta W  = (-1,0)
delta NW = (-1,-1)

-- |One step in the given direction, bounded by the dimension
deltaLoc :: (Int,Int) -> Direction -> (Int,Int)
deltaLoc (x,y) dir = (bound dim (x + dx), bound dim (y + dy))
    where
      (dx,dy) = delta dir

-- |Returns n wrapped into range 0-b
bound :: Int -> Int -> Int
bound b n | n' < 0 = n' + b
          | otherwise = n'
    where
      n' = rem n b    

-- Proof if proof were needed that more concise isn't necessarily good
wrand :: [Int] -> StdGen -> Int
wrand xs gen = do
  let (s,_) = randomR (0,sum xs) gen
      ys = filter (\(runningSum,_) -> s <= runningSum) $ zip (tail $ scanl (+) 0 xs) [0..]
  case ys of
    [] -> 0
    _ -> snd $ head ys

mkCell :: Int -> Double -> Cell
mkCell f p = Cell f p Nothing False

-- |Causes all the pheromones to evaporate a bit
evaporate :: World -> IO ()
evaporate w = atomically $ forM_ (elems (cells w)) (`updateTVar` evaporate')
    where
      evaporate' c = c {pheromone = pheromone c * evapRate}

updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar tv f = do
  v <- readTVar tv
  writeTVar tv (f v)

mkWorld :: IO World
mkWorld = atomically $ do
            cs <- replicateM ((1+dim)*(1+dim)) (newTVar (mkCell 0 0))
            return (World $ listArray ((0,0),(dim,dim)) cs)

populateWorld :: StdGen -> World -> IO [(Int,Int)]
populateWorld gen w = do
  -- Set up giant block of random numbers
  -- TODO factor this out into a monad (or use an existing one?)
  let dims       = take (2*foodPlaces) $ randomRs (0,dim) gen :: [Int]
      dirs       = randomRs (0,7) gen :: [Int]
      foodRanges = randomRs (0,foodRange) gen :: [Int]
      xy         = uncurry zip $ splitAt foodPlaces dims

  forM_ (zip3 [0..foodPlaces] xy foodRanges) 
        (\(_,p,f) -> atomically $ updateTVar (place w p) (\x -> x{ food = f }))
                                                            
  -- make some places at (x,y) that are home
  -- place an ant there facing in a random direction
  forM (zip [(x,y) | x <- homeRange, y <- homeRange] dirs)
       (\(p,dir) -> atomically $ updateTVar (place w p) 
                    (\x -> x { home = True, ant = Just (Ant (toEnum dir) False) }) >> return p)
                                                              

place :: World -> (Int,Int) -> TCell
place world (x,y) = cells world ! (x,y)

-- |Takes one food from current location.
-- TODO assert that..
-- 1) Food exists
-- 2) Ant exists
takeFood :: World -> (Int,Int) -> STM ()
takeFood w loc = do
  updateTVar p (\c -> c { food = pred (food c) })
  updateTVar p (\c -> c { ant = Just ((fromJust (ant c)) { hasFood = True }) } )
      where
        p = place w loc

-- |Drop food at current location
-- TODO assert that ant has food
dropFood :: World -> (Int,Int) -> STM ()
dropFood w loc = do 
  updateTVar p (\c -> c { food = succ (food c) } )
  updateTVar p (\c -> c { ant = Just ((fromJust (ant c)) { hasFood = False }) } )
      where
        p = place w loc

-- |Move the ant in the direction it is heading
-- TODO assert that the way is clear
move :: World -> (Int,Int) -> STM (Int,Int)
move w loc = do
  let src = place w loc
  cell <- readTVar src
  let dir    = direction $ fromJust $ ant cell
      newLoc = deltaLoc loc dir

  dest <- readTVar (cells w ! newLoc)

--  _ <- check (not (hasAnt dest))

  -- move the ant to the new cell
  updateTVar src clearAnt
  updateTVar (cells w ! newLoc) (\x -> x { ant = ant cell })

  -- Leave a trail
  unless (home cell) (updateTVar src incPher)
  return newLoc

-- BUG
turnAnt :: Int -> Cell -> Cell
turnAnt amt cell = cell { ant = Just turnedAnt } 
    where
      a = fromJust $ ant cell
      turnedAnt = a { direction = turnInt (direction a) amt }

turn :: World -> (Int,Int) -> Int -> STM ()
turn w loc amt = updateTVar src (turnAnt amt)
    where
      src = place w loc

-- | Map to their 1-based rank
rankBy :: (Cell -> Cell -> Ordering) -> [Cell] -> Map Cell Int
rankBy f xs = foldl (\m i -> M.insert (sorted !! i) (succ i) m) M.empty [0..length sorted - 1]
    where
      sorted = sortBy f xs

-- TODO much duplication to eliminate
forage :: StdGen -> World -> (Int,Int) -> STM (Int,Int)
forage gen w loc = do
  cell <- readTVar (place w loc)
  let a = fromJust $ ant cell
  ahead <- readTVar $ place w (deltaLoc loc (direction a))
  aheadLeft <- readTVar $ place w (deltaLoc loc (turnLeft (direction a)))
  aheadRight <- readTVar $ place w (deltaLoc loc (turnRight(direction a)))
  let places = [ahead,aheadLeft,aheadRight]
  if food cell > 0 && not (home cell) -- if there is food and we aren't at home
     then takeFood w loc >> turn w loc 4 >> return loc
     else if (food ahead > 0) && not (home ahead) && not (hasAnt ahead) -- food ahead and nothing in the way
          then move w loc 
          else do
            let f = rankBy (comparing food) places
                p = rankBy (comparing pheromone) places
                ranks = unionWith (+) f p -- TODO naff
                choice = wrand [if hasAnt ahead then 0 else ranks M.! ahead
                               ,ranks M.! aheadLeft
                               ,ranks M.! aheadRight] gen
                funcs = [move w
                        ,\x -> turn w x (- 1) >> return x
                        ,\x -> turn w x 1 >> return x]          
            ((funcs !! choice) loc)

-- 1:46minnutes http://blip.tv/file/812787
-- TODO much duplication to eliminate grass hopper
goHome :: StdGen -> World -> (Int,Int) -> STM (Int,Int)
goHome gen w loc = do
  cell <- readTVar (place w loc)
  let a = fromJust $ ant cell
  ahead <- readTVar $ place w (deltaLoc loc (direction a))
  aheadLeft <- readTVar $ place w (deltaLoc loc (turnLeft (direction a)))
  aheadRight <- readTVar $ place w (deltaLoc loc (turnRight(direction a)))
  let places = [ahead,aheadLeft,aheadRight]
  if home cell
     then dropFood w loc >> turn w loc 4 >> return loc -- drop food, turn around
     else if home ahead && not (hasAnt ahead)
          then move w loc -- head forward knowing the way is clear
          else do
            -- rank possibilities 
            -- by home is worth 1  #( => function literal
            -- by places sorted by pheromone content, merged with +
            -- vectors are functions of their indices
            -- [move, turnLeft, turnRight] are functions gotcha
            let p = rankBy (comparing pheromone) places 
                h = rankBy (comparing home) places
                ranks = unionWith (+) p h
                choice = wrand [if hasAnt ahead then 0 else ranks M.! ahead
                               ,ranks M.! aheadLeft
                               ,ranks M.! aheadRight] gen
                funcs = [move w
                        ,\x -> turn w x (- 1) >> return x
                        ,\x -> turn w x 1 >> return x]          
            ((funcs !! choice) loc)

-- | The main function for the ant agent
behave :: StdGen -> World -> (Int,Int) -> STM (Int,Int)
behave gen w loc = do
  cell <- readTVar (place w loc)
  let a = fromJust $ ant cell
  if hasFood a then goHome gen w loc else forage gen w loc  
                          
