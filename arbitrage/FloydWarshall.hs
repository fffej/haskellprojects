{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshall where

import Data.Maybe
import Data.Array
import Data.List (maximumBy)

class Enum b => Graph a b | a -> b where
    vertices ::  a -> [b]
    edge :: a -> b -> b -> Maybe Double
    fromInt :: a -> Int -> b

-- An arbitrary representation of infinity!
infinity :: Double
infinity = 1000000

findArbitrage :: Graph a b => a -> Maybe [b]
findArbitrage g | Nothing == maybePath = Nothing
                | otherwise = Just sq
    where
      res = floydWarshall g
      maybePath = arbChances res
      sq = map (fromInt g) (steps res (fst $ fromJust maybePath))

-- When the steps is zero it's simply the edge weights
type FWResult = Array (Int,Int,Int) (Double,Int)

floydWarshall :: Graph a b => a -> FWResult
floydWarshall g = arr
    where 
      arr = array ((0,0,0),(n,n,n)) [((m,i,j),f m i j) | 
                                     m <- [0..n],
                                     i <- [0..n],
                                     j <- [0..n]]
      n = length (vertices g) - 1 
      f = floydWarshallStep g arr 
      

floydWarshallStep :: Graph a b => a -> FWResult -> Int -> Int -> Int -> (Double,Int)

-- |The base case simply initializes to the edges
floydWarshallStep g _ 0 i j | i == j = (1,-1)
                            | otherwise = (d,-1)
    where
      w = edge g (fromInt g i) (fromInt g j)
      d = maybe infinity (const (fromJust w)) w

-- |The recursive case is defined in terms of the previous ones
floydWarshallStep g prev m i j = (bestVal,pathVal)
    where
      n = length (vertices g) - 1 
      bests1 = [fst $ prev ! (m-1,i,k) | k <- [0..n]]
      bests2 = [fst $ prev ! (0,k,j) | k <- [0..n]]
      bests = zip (zipWith (*) bests1 bests2) [0..n] -- maximum value
      (bestVal,pathVal) = maximumBy (\(v,i) (x,j) -> compare v x) bests


arbChances :: FWResult -> Maybe ((Int,Int,Int),(Double,Int))
arbChances a | null c = Nothing
             | otherwise = Just (head c)
    where
      c = filter (\((steps,i,j),(best,path)) -> steps > 1 && i == j && best > 1.01) (assocs a)

-- #steps i to j
steps :: FWResult -> (Int,Int,Int) -> [Int]
steps a (s,i,j) = i : steps' a (s,i,j) ++ [i]

steps' :: FWResult -> (Int,Int,Int) -> [Int]
steps' a (1,i,j) = []
steps' a (s,i,j) = v : (steps' a (s - 1,i,v))
    where
      v = snd $ a ! (s,i,j)




