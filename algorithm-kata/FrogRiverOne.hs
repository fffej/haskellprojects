module FrogRiverOne where

{- A small frog wants to get to the other side of a river. The frog is initially located on one bank of the river (position 0) and wants to get to the opposite bank (position X+1). Leaves fall from a tree onto the surface of the river.

You are given a zero-indexed array A consisting of N integers representing the falling leaves. A[K] represents the position where one leaf falls at time K, measured in seconds.

The goal is to find the earliest time when the frog can jump to the other side of the river. The frog can cross only when leaves appear at every position across the river from 1 to X (that is, we want to find the earliest moment when all the positions from 1 to X are covered by leaves). You may assume that the speed of the current in the river is negligibly small, i.e. the leaves do not change their positions once they fall in the river. -}

import Data.Bits (setBit)
import Data.List (elemIndex)

solve :: Int -> [Int] -> Maybe Int
solve a xs = elemIndex ((2^a) - 1) pop 
  where
    pop :: [Integer]
    pop = scanl setBit 1 xs

exampleA :: [Int]
exampleA = [1,3,1,4,2,3,5,4]


