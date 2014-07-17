module DynamicTimeWarping where

import Data.Array hiding ((!))
import Data.Array.ST (runSTArray, newArray, readArray, writeArray)

import Data.Vector ((!))
import qualified Data.Vector as V

import Control.Monad (forM_)

import Data.Word (Word8)
import Codec.BMP
import qualified Data.ByteString as BS

intCost :: Int -> Int -> Int
intCost x y = abs (x - y)

maxcost :: Int
maxcost = maxBound

dtw :: V.Vector a -> V.Vector a -> (a -> a -> Int) -> Array (Int,Int) Int
dtw x y cost = runSTArray $ do
  let n = V.length x
      m = V.length y
  d <- newArray ((0,0),(m,n)) 0
  forM_ [1..n] (\i -> writeArray d (0,i) maxcost)
  forM_ [1..m] (\i -> writeArray d (i,0) maxcost)
  forM_ [1..m] $ \i -> 
    forM_ [1..n] $ \j -> do
      let c = cost (x ! (i -1)) (y ! (j -1))
      insertion <- readArray d (j,i-1)
      deletion <- readArray d (j-1,i)
      match <- readArray d (j-1,i-1)
      writeArray d (j,i) (c + minimum [insertion,deletion,match])
  return d

render :: Array (Int,Int) Int -> FilePath -> IO ()
render arr file = do
  print w
  print h
  print (length (elems arr))
  print (BS.length bs)
  writeBMP file bmp
    where
      ((_,_),(w,h)) = bounds arr
      bs = BS.pack (concatMap (normalize 0 (maximum vs)) vs)
      bmp = packRGBA32ToBMP w h bs
      vs = map snd $ filter (\((x,y),_) -> x /= 0 && y /= 0) (assocs arr)

-- TODO http://stackoverflow.com/questions/7706339/grayscale-to-red-green-blue-matlab-jet-color-scale
normalize :: Int -> Int -> Int -> [Word8]
normalize minx maxx x = [scale r, scale g, scale b, 0]
  where
    (r,g,b) = color normalized
    scale v = floor (maxB * v)
    normalized = delta / range
    maxB = fromIntegral (maxBound :: Word8)
    delta = fromIntegral $ x - minx
    range = fromIntegral $ maxx - minx

-- v is bound between 0 and 1
-- dv is 1
color :: Double -> (Double,Double,Double)
color v
  | v < 0.25  = (0,4*v,1)
  | v < 0.50  = (0,1,1 + 4 * (0.25 - v))
  | v < 0.75  = (4 * (v - 0.5),0,1)
  | otherwise = (1,1 + 4 * (0.75 - v),1)
    

example1 :: IO ()
example1 = do
  let seq1 = [0 .. 500]
      seq2 = [0 .. 500]
      cost = dtw (V.fromList seq1) (V.fromList seq2) intCost
  render cost "exact.bmp"
