{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FloydWarshall where

import Data.Maybe
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

import Control.Exception
import Control.Monad (forM,forM_,liftM,liftM2,liftM3,when)

data Vertex a = Vertex a    
              deriving (Show)

-- A mutable vector represents the 3D array
type DVector = M.IOVector Double
type IVector = M.IOVector Int

-- which carries around the bounds
data Array = Array Int DVector IVector

-- Use this to break out of the loop (vomit copiously, but want to get
-- the code working like the C code before functionalizing it)
data ArbitrageFound = ArbitrageFound Int Int
                      deriving(Show, Typeable)

instance Exception ArbitrageFound

class Enum b => Graph a b | a -> b where
    vertices ::  a -> [Vertex b]
    edge :: a -> Vertex b -> Vertex b -> Maybe Double
    fromInt :: a -> Int -> Vertex b

-- An arbitrary representation of infinity!
infinity :: Double
infinity = 1000000

createArray :: Int -> IO Array
createArray n = liftM2 (Array n) (GM.newWith n3 0) (GM.newWith n3 (- 1)) where
    n3 = n*n*n

-- |Fill up the arrays based on the data contained in the graph
initializeArray :: (Graph a b) => a -> IO Array
initializeArray g = do
  let n = (length $ vertices g) - 1
  arr <- createArray (n+1)
  forM_ [0..n]
        (\i -> forM_ [0..n]
         (\j -> do -- TODO simplify
            let w = edge g (fromInt g i) (fromInt g j)
                v = maybe infinity (const (fromJust w)) w          
            writeVal arr (0,i,j) v))
  return arr

getPath :: (Graph a b) => ArbitrageFound -> a -> Array -> IO [b]
getPath (ArbitrageFound steps i) g a = do
  let Vertex v = fromInt g i
  j <- readPath a (steps,i,i)
  _ <- printArray g a
  rest <- getPath' g a i (steps - 1) j
  return (v:rest)

getPath' :: (Graph a b) => a -> Array -> Int -> Int -> Int -> IO [b]
getPath' g a i 0 j = return [v] 
    where
      (Vertex v) = fromInt g i

getPath' g a i steps j = do
  print (steps,i,j)
  pathVal <- readPath a (steps,i,j)
  let (Vertex v) = fromInt g pathVal
  vs <- getPath' g a i (steps - 1) pathVal
  return (v:vs)
  
findArbitrage :: (Graph a b) => a -> IO (Maybe [b])
findArbitrage g = do
  a <- initializeArray g
  result <- floydWarshall g a
  case result of
    Nothing -> return Nothing
    Just i -> liftM Just (getPath i g a)               

floydWarshall :: Graph a b => a -> Array -> IO (Maybe ArbitrageFound)
floydWarshall g arr = do
  let n = (length $ vertices g) - 1
  let x = forM_ [1..n]
          (\m -> forM_ [0..n]
           (\i -> forM_ [0..n]
            (\j -> forM_ [0..n]
             (\k -> do
                mij <- readVal arr (m,i,j)
                temp <- liftM2 (*) (readVal arr (m - 1,i,k)) (readVal arr (0,k,j))
                when (mij < temp) 
                         (writeVal arr (m,i,j) temp >> writePath arr (m,i,j) k)
                currentVal <- readVal arr (m,i,i)
                when (currentVal > 1.01) 
                         (throw (ArbitrageFound (1 + m) i))))))
  handle (\i -> return $ Just i) (x >> return Nothing)

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