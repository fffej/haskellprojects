module Ants where

import Data.Maybe

import Data.Array

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

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
               deriving (Enum,Show)

nextDir :: Direction -> Direction
nextDir NW = N
nextDir x = succ x

data Ant = Ant {
      direction :: Direction
    , hasFood :: Bool
}

data Cell = Cell {
      food :: Int
    , pheromone :: Double
    , ant :: Maybe Ant
    , home :: Bool
}

data Agent = Agent Ant

{- Boring helper functions -}

clearAnt :: Cell -> Cell
clearAnt cell = cell { ant = Nothing }

incPher :: Cell -> Cell
incPher cell = cell { pheromone = succ (pheromone cell) }

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

mkCell :: Int -> Double -> Cell
mkCell f p = Cell f p Nothing False

evaporate' :: Cell -> Cell
evaporate' c = c { pheromone = pheromone c * evapRate }

-- |Causes all the pheromones to evaporate a bit
evaporate :: World -> IO ()
evaporate w = atomically $ forM_ (elems (cells w)) (`updateTVar` evaporate')

updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar tv f = do
  v <- readTVar tv
  writeTVar tv (f v)

mkWorld :: STM World
mkWorld = do
  cells <- replicateM (dim*dim) (newTVar (mkCell 0 0))
  return (World $ listArray ((0,0),(dim,dim)) cells)

place :: World -> (Int,Int) -> TCell
place world (x,y) = cells world ! (x,y)

-- |Takes one food from current location.
-- TODO assert that..
-- 1) Food exists
-- 2) Ant exists
takeFood :: World -> (Int,Int) -> IO ()
takeFood w loc = atomically $ do
                   updateTVar p (\c -> c { food = pred (food c) })
                   updateTVar p (\c -> c { ant = Just ((fromJust (ant c)) { hasFood = True }) } )
    where
      p = place w loc

-- |Drop food at current location
-- TODO assert that ant has food
dropFood :: World -> (Int,Int) -> IO ()
dropFood w loc = atomically $ do 
                   updateTVar p (\c -> c { food = succ (food c) } )
                   updateTVar p (\c -> c { ant = Just ((fromJust (ant c)) { hasFood = False }) } )
    where
      p = place w loc

-- |Move the ant in the direction it is heading
-- TODO assert that the way is clear
move  :: World -> (Int,Int) -> IO (Int,Int)
move w loc = atomically $ do
               cell <- readTVar src
               let dir    = direction $ fromJust $ ant cell
                   newLoc = deltaLoc loc dir
               dest <- readTVar (cells w ! newLoc)
               -- move the ant to the new cell
               updateTVar src clearAnt
               updateTVar src (\x -> x { ant = ant cell })
               -- Leave a trail
               when (home cell) (updateTVar src incPher)
               return newLoc
    where
      src = place w loc

turnAnt :: Int -> Cell -> Cell
turnAnt amt cell = cell { ant = Just turnedAnt } 
    where
      a = fromJust $ ant cell
      turnedAnt = a { direction = nextDir (direction a) }

turn :: World -> (Int,Int) -> Int -> IO ()
turn w loc amt = atomically $ updateTVar src (turnAnt amt)
    where
      src = place w loc

forage :: World -> (Int,Int) -> STM (Int,Int)
forage w loc = undefined

goHome :: World -> (Int,Int) -> STM (Int,Int)
goHome w loc = undefined
-- If we are home, drop food and turn around
-- Otherwise search based on pheromene
-- Return new location

-- | The main function for the ant agent
behave :: World -> (Int,Int) -> IO (Int,Int)
behave w loc = atomically $ do
                 cell <- readTVar (place w loc)
                 let a = fromJust $ ant cell
                 if (hasFood a)
                    then forage w loc
                    else goHome w loc
                           
    
      
               


{- notes about stm
  
  locks and condition variables do not support modular programming

  atomically makes two guarantees
  - atomicity - change becomes visible in the whole, not in parts
  - isolation - act is unaffected by any other threads

 -- It is therefore good library design to export STM actions (rather than IO actions)
whenever possible, because they are composable; their type advertises that they
do no irrevocable effects.

-}
