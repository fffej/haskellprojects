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

doubleCost :: Double -> Double -> Int
doubleCost x y = floor $ abs (x - y) * 10.0

dtw :: V.Vector a -> V.Vector a -> (a -> a -> Int) -> Array (Int,Int) Int
dtw x y cost = runSTArray $ do
  let n = V.length x
      m = V.length y
      maxcost = maxBound
  d <- newArray ((0,0),(m,n)) 0
  forM_ [1..n] (\i -> writeArray d (0,i) maxcost)
  forM_ [1..m] (\i -> writeArray d (i,0) maxcost)
  forM_ [1..n] $ \i -> 
    forM_ [1..m] $ \j -> do
      let c = cost (x ! (i -1)) (y ! (j -1))
      insertion <- readArray d (j,i-1)
      deletion <- readArray d (j-1,i)
      match <- readArray d (j-1,i-1)
      writeArray d (j,i) (c + minimum [insertion,deletion,match])
  return d

dtwWin :: V.Vector a -> V.Vector a -> (a -> a -> Int) -> Int -> Array (Int,Int) Int
dtwWin x y cost window = runSTArray $ do
  let n = V.length x
      m = V.length y
      maxCost = maxBound
      w = max window (abs (n - m)) -- constrain window size
  d <- newArray ((0,0),(m,n)) maxCost
  writeArray d (0,0) 0
  forM_ [1..n] $ \i ->
    forM_ [max 1 (i-w) .. min m (i+w)] $ \j -> do
      let c = cost (x ! (i - 1)) (y ! (j - 1))
      insertion <- readArray d (j,i-1)
      deletion <- readArray d (j-1,i)
      match <- readArray d (j-1,i-1)
      writeArray d (j,i) (c + minimum [insertion,deletion,match])
  return d

render :: Array (Int,Int) Int -> FilePath -> IO ()
render arr file = writeBMP file bmp
  where
    ((_,_),(w,h)) = bounds arr
    bs = BS.pack (concatMap (normalize 0 maxvs) vs)
    bmp = packRGBA32ToBMP (w+1) (h+1) bs
    vs = elems arr
    maxvs = maximum (filter (/= (maxBound :: Int)) vs)

-- TODO http://stackoverflow.com/questions/7706339/grayscale-to-red-green-blue-matlab-jet-color-scale
normalize :: Int -> Int -> Int -> [Word8]
normalize minx maxx x = [scale r, scale g, scale b, 0]
  where
    (r,g,b) = color normalized
    scale v = floor (maxB * v)
    normalized = delta / rnge
    maxB = fromIntegral (maxBound :: Word8)
    delta = fromIntegral $ x - minx
    rnge = fromIntegral $ maxx - minx

-- v is bound between 0 and 1
-- dv is 1
color :: Double -> (Double,Double,Double)
color v
  | v < 0.25  = (0,4*v,1)
  | v < 0.50  = (0,1,1 + 4 * (0.25 - v))
  | v < 0.75  = (4 * (v - 0.5),0,1)
  | otherwise = (1,1 + 4 * (0.75 - v),1)
    

save :: [Int] -> [Int] -> FilePath -> IO ()
save seq1 seq2 filename = do
  let cost = dtw (V.fromList seq1) (V.fromList seq2) intCost
  render cost filename

saveDouble :: [Double] -> [Double] -> FilePath -> IO ()
saveDouble seq1 seq2 filename = do
  let cost = dtw (V.fromList seq1) (V.fromList seq2) doubleCost
  render cost filename


saveWin :: [Int] -> [Int] -> Int -> FilePath -> IO ()
saveWin seq1 seq2 w filename = do
  let cost = dtwWin (V.fromList seq1) (V.fromList seq2) intCost w
  render cost filename

cosInt :: [Int]
cosInt = map (floor . (*10) . cos) [-300 .. 300]

sinInt :: [Int]
sinInt = map (floor . (*10). sin) [-300 .. 300]

main :: IO ()
main = do
  saveWin [0..500] [0..500] 5 "perfect-win5.bmp"
  save    [0..500] [0..500]   "perfect.bmp"
  saveWin [0..500] [500,499..0] 5 "opposite-win5.bmp"
  save    [0..500] [500,499..0] "opposite.bmp" 
  saveWin [0..500] [2,4..1000] 5 "double-win5.bmp"
  save    [0..500] [2,4..1000] "double.bmp"
  save    cosInt   sinInt "cos-sin.bmp"
  save    cosInt  [0..500] "cosInt-Linear.bmp"
