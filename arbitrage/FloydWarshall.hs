{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshall where

import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

import Control.Monad (forM_,liftM2,liftM3,when)

data Vertex a = Vertex a    
              deriving (Show)

-- A mutable vector of doubles represents the 2D array
type DVector = M.IOVector Double
type IVector = M.IOVector Int

-- which carries around the bounds
data Array = Array Int DVector IVector

class Enum b => Graph a b | a -> b where
    vertices ::  a -> [Vertex b]
    edge :: a -> Vertex b -> Vertex b -> Maybe Double
    fromInt :: a -> Int -> Vertex b

-- An arbitrary representation of infinity!
infinity :: Double
infinity = 1000000

createArray :: Int -> IO Array
createArray n = liftM2 (Array n) (GM.newWith n3 0) (GM.newWith n3 (- 1)) where
    n3 = n * n * n

-- |Fill up the arrays based on the data contained in the graph
initializeArray :: (Graph a b) => a -> IO Array
initializeArray g = do
  arr <- createArray (length (vertices g))
  let n = (length $ vertices g) - 1
  forM_ [0..n]
        (\i -> forM_ [0..n]
         (\j -> do
            let w = edge g (fromInt g i) (fromInt g j)
                v = maybe infinity (const (fromJust w)) w          
            writeVal arr (0,i,j) v))
  return arr

floydWarshall :: Graph a b => a -> Array -> IO ()
floydWarshall g arr = do
  let n = (length $ vertices g) - 1
  forM_ [1..n]
            (\m -> forM_ [0..n]
             (\i -> forM_ [0..n]
              (\j -> forM_ [0..n]
               (\k -> do
                  mij <- readVal arr (m,i,j)
                  temp <- liftM2 (*) (readVal arr (m - 1,i,k)) (readVal arr (0,k,j))
                  when (mij < temp) (writeVal arr (m,i,j) temp >> writePath arr (m,i,j) k)))))

printArray :: Graph a b => a -> Array -> IO ()
printArray g arr = do
  let n = (length $ vertices g) - 1
  forM_ [0..n]
             (\k -> forM_ [0..n]
              (\i -> forM_ [0..n]
               (\j -> do
                  x <- readVal arr (k,i,j)
                  y <- readPath arr (k,i,j)            
                  print (show (k,i,j) ++ "," ++ show x ++ "," ++ show y))))

ix :: Int -> (Int,Int,Int) -> Int
ix n (i,j,k) = i*n*n + j*n + k

writeVal :: Array -> (Int,Int,Int) -> Double -> IO ()
writeVal (Array n vec _) p = GM.write vec (ix n p)

writePath :: Array -> (Int,Int,Int) -> Int -> IO ()
writePath (Array n _ vec) p = GM.write vec (ix n p)

readVal :: Array -> (Int,Int,Int) -> IO Double 
readVal (Array n vec _) p = GM.read vec (ix n p)

readPath :: Array -> (Int,Int,Int) -> IO Int
readPath (Array n _ vec) p = GM.read vec (ix n p)

