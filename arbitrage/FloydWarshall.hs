{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshall where

import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

import Control.Monad (forM_,liftM,liftM2)

data Vertex a = Vertex a    
              deriving (Show)

-- A mutable vector of doubles represents the 2D array
type DVector = M.IOVector Double

-- which carries around the bounds
data Array = Array Int DVector

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
  return (Array n2 v)

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
              val <- (liftM2 min) (readVal arr (i,j)) (liftM2 (+) (readVal arr (i,k)) (readVal arr (k,j)))
              _ <- writeVal arr (i,j) val
              return ()))))

ix :: Int -> (Int,Int) -> Int
ix n (i,j) = i*n + j

writeVal :: Array -> (Int,Int) -> Double -> IO ()
writeVal (Array n vec) p = GM.unsafeWrite vec (ix n p)

readVal :: Array -> (Int,Int) -> IO Double 
readVal (Array n vec) p = GM.unsafeRead vec (ix n p)

