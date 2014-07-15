module DynamicTimeWarping where

import Data.Array hiding ((!))
import Data.Array.ST

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V

import Control.Monad

cost :: Int -> Int -> Int
cost x y = abs (x - y)

maxcost :: Int
maxcost = 100001

dtw :: V.Vector Int -> V.Vector Int -> Array (Int,Int) Int
dtw x y = runSTArray $ do
  let n = V.length x
      m = V.length y
  d <- newArray ((0,0),(n,m)) 0
  forM_ [0..n] $ \i -> do
    forM_ [0..m] $ \j -> do
      let c = cost (x ! i) (y ! j)
      insertion <- readArray d (i-1,j)
      deletion <- readArray d (i,j-1)
      match <- readArray d (i-1,j-1)
      writeArray d (i,j) (c + minimum [insertion,deletion,match])
  return d
