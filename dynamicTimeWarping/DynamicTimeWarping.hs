module DynamicTimeWarping where

import Data.Array hiding ((!))
import Data.Array.IArray (amap)
import Data.Array.ST (runSTArray, newArray, readArray, writeArray)

import Data.Vector ((!))
import qualified Data.Vector as V

import Control.Monad (forM_)

import Data.Word (Word16)
import Graphics.Pgm (arrayToFile)

intCost :: Int -> Int -> Int
intCost x y = abs (x - y)

maxcost :: Int
maxcost = 100001

dtw :: V.Vector a -> V.Vector a -> (a -> a -> Int) -> Array (Int,Int) Int
dtw x y cost = runSTArray $ do
  let n = V.length x
      m = V.length y
  d <- newArray ((0,0),(n,m)) 0
  forM_ [1..n] $ \i -> 
    forM_ [1..m] $ \j -> do
      let c = cost (x ! (i -1)) (y ! (j -1))
      insertion <- readArray d (i-1,j)
      deletion <- readArray d (i,j-1)
      match <- readArray d (i-1,j-1)
      writeArray d (i,j) (c + minimum [insertion,deletion,match])
  return d

render :: Array (Int,Int) Int -> FilePath -> IO ()
render arr file = arrayToFile file (amap (normalize 0 255) arr)

normalize :: Int -> Int -> Int -> Word16
normalize minx maxx x = fromIntegral $ (maxx - x) `div` r
  where
    r = maxx - minx
