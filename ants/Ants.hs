{-# LANGUAGE BangPatterns #-}
module Ants where

import Control.Monad
import Control.Concurrent.STM

import Data.Ord (comparing)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)
import Data.Map (Map,unionWith)
import qualified Data.Map as M

import System.Random

--import Criterion
--import Criterion.Main

-- |Dimensions of square world
dim :: Int
dim = 80

-- |Number of ants
nantsSqrt :: Int
nantsSqrt = 10

-- |Number of places with food
foodPlaces :: Int
foodPlaces = 100

-- |Range of amount of food at a place
foodRange :: Int
foodRange = 100

-- |Evaporation rate
evapRate :: Double
evapRate = 0.9999999

homeOff :: Int
homeOff = dim `div` 4

type TCell = TVar Cell

type TCellArray = Vector TCell

type World = TCellArray

data Ant = Ant {
      direction :: Direction
    , hasFood :: Bool
} deriving (Eq,Show)
 
data Cell = Cell {
      food :: !Int
    , pher :: !Double
    , ant :: Maybe Ant
    , home :: !Bool
} deriving (Eq,Show)

instance Ord Cell where
    compare = comparing food

data Direction = N | NE | E | SE | S | SW | W | NW
               deriving (Enum,Show,Eq)

turnRight :: Direction -> Direction
turnRight NW = N
turnRight x = succ x

turnLeft :: Direction -> Direction
turnLeft N = NW
turnLeft x = pred x

turnInt :: Int -> Direction -> Direction
turnInt 0 d = d
turnInt x d | x < 0     = turnInt (x + 1) (turnRight d)
            | otherwise = turnInt (x - 1) (turnLeft d)

{- Boring helper functions -}
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

wrand :: [Int] -> StdGen -> Int
wrand xs gen = f 0 0
    where 
      total = sum xs 
      (r,_) = randomR (0,total - 1) gen
      f = \i sum -> if r < (xs !! i) + sum 
                    then i
                    else f (succ i) (sum + (xs !! i))

-- |Causes all the phers to evaporate a bit
evaporate :: World -> STM ()
evaporate w = V.forM_ w (\x -> updateTVar x (\c -> c { pher = pher c * evapRate }) `seq` return ())

updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar !tv f = do
  x <- readTVar tv
  _ <- writeTVar tv $! (f x)
  return ()
                                                       
place :: World -> (Int,Int) -> TCell
place world (x,y) = world V.! (x*dim + y)

-- |Must be called in a transaction where has food at loc
takeFood :: World -> (Int,Int) -> STM ()
takeFood w loc = do
  src <- readTVar p
  updateTVar p (\c -> c { food = pred (food c) 
                        , ant = Just ((fromJust (ant c)) { hasFood = True }) })
      where
        p = place w loc

-- |Must be called in a transaction where the ant has food
dropFood :: World -> (Int,Int) -> STM ()
dropFood w loc = do 
  src <- readTVar p
  updateTVar p (\c -> c { food = succ (food c) 
                        , ant = Just ((fromJust (ant c)) { hasFood = False }) })
      where
        p = place w loc

-- |Move the ant in the direction it is heading
move :: World -> (Int,Int) -> STM (Int,Int)
move w loc = do
  let src = place w loc
  cell <- readTVar src
  let dir    = direction $ fromJust $ ant cell
      newLoc = deltaLoc loc dir

  -- Is the coast clear?
  dest <- readTVar (place w newLoc)
  _ <- check (not (hasAnt dest))

  -- move the ant to the new cell
  updateTVar src (\x -> x { ant = Nothing } )
  updateTVar (place w newLoc) (\x -> x { ant = ant cell })

  -- Leave a trail
  unless (home cell) 
      (updateTVar src (\x -> x { pher = succ $ pher x } ))
  return newLoc

-- |Must be called when asserted there is an ant
turnAnt :: Int -> Cell -> Cell
turnAnt amt cell = cell { ant = Just turnedAnt } 
    where
      a = fromJust $ ant cell      
      turnedAnt = a { direction = turnInt amt (direction a) }

-- |Must be called when true that world (int,int) is an ant
turn :: World -> (Int,Int) -> Int -> STM ()
turn w loc amt = do
  cell <- readTVar src
  updateTVar src (turnAnt amt)
    where
      src = place w loc

-- | Map to their 1-based rank
rankBy :: (Cell -> Cell -> Ordering) -> [Cell] -> Map Cell Int
rankBy f xs = foldl (\m i -> M.insert (sorted !! i) (succ i) m) M.empty [0..length sorted - 1]
    where
      sorted = sortBy f xs

-- | The main function for the ant agent
behave :: StdGen -> World -> (Int,Int) -> STM (Int,Int)
behave gen w loc = do
  cell <- readTVar (place w loc)
  let a = fromJust $ ant cell
  ahead <- readTVar $ place w (deltaLoc loc (direction a))
  aheadLeft <- readTVar $ place w (deltaLoc loc (turnLeft (direction a)))
  aheadRight <- readTVar $ place w (deltaLoc loc (turnRight(direction a)))
  let places = [ahead,aheadLeft,aheadRight]
      p = rankBy (comparing pher) places
      f = rankBy (comparing food) places
      h = rankBy (comparing home) places
      ranks = if hasFood a then unionWith (+) p h else unionWith (+) f p
      choice = wrand [if hasAnt ahead then 0 else ranks M.! ahead
                     ,ranks M.! aheadLeft
                     ,ranks M.! aheadRight] gen
      action = [move w
               ,\x -> turn w x 1 >> return x
               ,\x -> turn w x (- 1) >> return x]  !! choice
  if hasFood a 
     then if home cell
             then dropFood w loc >> turn w loc 4 >> return loc -- drop food, turn around
             else if home ahead && not (hasAnt ahead)
                  then move w loc -- head forward knowing the way is clear
                  else action loc
     else if food cell > 0 && not (home cell) -- if there is food and we aren't at home
             then takeFood w loc >> turn w loc 4 >> return loc
             else if (food ahead > 0) && not (home ahead) && not (hasAnt ahead) -- food ahead and nothing in the way
                  then move w loc 
                  else action loc

                          
mkCell :: Int -> Double -> Cell
mkCell f p = Cell f p Nothing False

mkWorld :: IO (World,[(Int,Int)])
mkWorld = do 
  gen <- getStdGen

  w <- atomically $ do
            cs <- replicateM ((1+dim)*(1+dim)) (newTVar (mkCell 0 0))
            return (V.fromList cs)

  let dims       = take (2*foodPlaces) $ randomRs (0,dim) gen :: [Int]
      dirs       = randomRs (0,7) gen :: [Int]
      foodRanges = randomRs (0,foodRange) gen :: [Int]
      xy         = uncurry zip $ splitAt foodPlaces dims

  -- Position the food randomly
  forM_ (zip3 [0..foodPlaces] xy foodRanges) 
        (\(_,p,f) -> atomically $ updateTVar (place w p) (\x -> x{ food = f }))
                               
  -- Set up the home area
  ants <- forM (zip [(x,y) | x <- homeRange, y <- homeRange] dirs)
          (\(p,dir) -> atomically $ updateTVar (place w p) 
           (\x -> x { home = True, ant = Just (Ant (toEnum dir) False) }) >> return p)

  return (w,ants)

-- |Just for debugging
countAnts :: World -> STM Int
countAnts w = liftM V.length (V.filterM (\x -> fmap hasAnt (readTVar x)) w)

getAnts :: World -> STM [Ant]
getAnts w = liftM (catMaybes . V.toList) $ V.mapM (\x -> fmap ant (readTVar x)) w

-- Performance benchmarks
{-main = do
  (world,(ant:ants)) <- mkWorld
  let gen = mkStdGen 101
  defaultMain [
        bgroup "Ants" [ bench "evaporate" $ whnfIO $ atomically $ evaporate world ] 
       ]-}
--                     , bench "behave" $ whnfIO $ atomically $ behave gen world ant]

