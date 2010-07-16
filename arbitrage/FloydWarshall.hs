{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshall where

import Data.Maybe
import Data.Array

class Enum b => Graph a b | a -> b where
    vertices ::  a -> [b]
    edge :: a -> b -> b -> Maybe Double
    fromInt :: a -> Int -> b

-- An arbitrary representation of infinity!
infinity :: Double
infinity = 10000000

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
floydWarshallStep g _ 0 i j | i == j = (1.0,-1)
                            | otherwise = (d,-1)
    where
      w = edge g (fromInt g i) (fromInt g j)
      d = maybe infinity (const (fromJust w)) w

-- |The recursive case is defined in terms of the previous ones
floydWarshallStep g a m i j = foldl f (0.0,-1) [0..n]
    where
      n = length (vertices g) - 1 
      f :: (Double,Int) -> Int -> (Double,Int)
      f (b,p) k | b < (mik*okj) = (mik*okj,k)
              | otherwise = (b,p)
        where
          mik = fst $ a ! (m-1,i,k)
          okj = fst $ a ! (0,k,j)


arbChances :: FWResult -> Maybe ((Int,Int,Int),(Double,Int))
arbChances a | null c = Nothing
             | otherwise = Just (head c)
    where
      c = filter (\((s,i,j),(best,_)) -> s >= 1 && i == j && best > 1.01 && best < infinity) 
          (assocs a)

steps :: FWResult -> (Int,Int,Int) -> [Int]
steps a (s,i,j) = reverse $ i : x  : steps' a (s - 1,i,x) ++ [i]
    where
      x = snd $ a ! (s,i,j)

steps' :: FWResult -> (Int,Int,Int) -> [Int]
steps' _ (0,_,_) = []
steps' a (s,i,j) = p : steps' a (s-1,i,p)
    where
      p = snd $ a ! (s,i,j)



