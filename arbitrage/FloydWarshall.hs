{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshall where

import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

import Control.Monad (forM_,liftM2)

data Vertex a = Vertex a    
              deriving (Show)

-- A mutable vector of doubles represents the 2D array
type DVector = M.IOVector Double

-- which carries around the bounds
data Array = Array Int DVector DVector

class Enum b => Graph a b | a -> b where
    vertices ::  a -> [Vertex b]
    edge :: a -> Vertex b -> Vertex b -> Maybe Double
    fromInt :: a -> Int -> Vertex b

-- An arbitrary representation of infinity!
infinity :: Double
infinity = 1000000

createArray :: Int -> IO Array
createArray n = do
  let n2 = n*n 
  v <- GM.unsafeNewWith n2 0
  p <- GM.unsafeNewWith n2 0
  return (Array n2 v p)

initializeArray :: (Graph a b) => a -> IO Array
initializeArray g = do
  arr <- createArray (length (vertices g))
  let n = length $ vertices g
  forM_ [0..n]
        (\i -> forM_ [0..n]
         (\j -> forM_ [0..n]
          (\_ -> do
             let w = edge g (fromInt g i) (fromInt g j)
                 v = maybe infinity (const (fromJust w)) w
             writeVal arr (i,j) 0.0))) -- TODO lookup the weight
  return arr

floydWarshall :: Graph a b => a -> Array -> IO ()
floydWarshall g arr = do
  let n = length $ vertices g
  forM_ [0..n]
        (\k -> forM_ [0..n]
         (\i -> forM_ [0..n]
          (\j -> forM_ [0..n]
           (\_ -> do
              ij <- readVal arr (i,j)
              ikkj <- liftM2 (+) (readVal arr (i,k)) (readVal arr (k,j))
              _ <- if (ikkj < ij) then (writeVal arr (i,j) ikkj) else (writePath arr (i,j) (fromIntegral k))
              return ()))))

reconstructPath :: Graph a b => a -> Array -> IO [b]
reconstructPath = undefined

ix :: Int -> (Int,Int) -> Int
ix n (i,j) = i*n + j

writeVal :: Array -> (Int,Int) -> Double -> IO ()
writeVal (Array n vec _) p = GM.unsafeWrite vec (ix n p)

writePath :: Array -> (Int,Int) -> Double -> IO ()
writePath (Array n _ vec) p = GM.unsafeWrite vec (ix n p)

readVal :: Array -> (Int,Int) -> IO Double 
readVal (Array n vec _) p = GM.unsafeRead vec (ix n p)

readPath :: Array -> (Int,Int) -> IO Double
readPath (Array n _ vec) p = GM.unsafeRead vec (ix n p)

