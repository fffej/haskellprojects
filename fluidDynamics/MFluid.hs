module MFluid (main) where
-- Inspired by http://www.bestinclass.dk/index.php/2010/03/functional-fluid-dynamics-in-clojure/
-- http://github.com/LauJensen/Fluid-Dynamics/raw/master/fluids.clj
-- Navier Stokes (http://en.wikipedia.org/wiki/Navier–Stokes_equations)

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic         as G

import Criterion.Main
import Test.HUnit
import Data.List (foldl')

import Control.Monad

type DVector = M.IOVector Double

data Grid = Grid Int DVector

instance Show Grid where
    show (Grid n g) = "Grid: " ++ show n
                     
vecToList :: DVector -> IO [Double]
vecToList d = mapM (M.read d) [0..n] where
    n = M.length d - 1

absDifference :: [Double] -> [Double] -> Double
absDifference v1 v2 = sqrt (sum (map (\y -> y*y) (zipWith (-) v1 v2)))

nearlyEqual :: [Double] -> [Double] -> Bool
nearlyEqual x y = absDifference x y < 0.0001

gridToList :: Grid -> IO [Double]
gridToList (Grid _ d) = vecToList d

vectorLength :: Int -> Int
vectorLength sz = (sz+2)*(sz+2)

listToVec :: [Double] -> IO DVector
listToVec d = do
    let n = length d
    v <- GM.unsafeNewWith n 0.0
    mapM_ (\(x,p) -> M.write v p x) (zip d [0..])
    return v

zeroGrid :: Grid -> IO ()
zeroGrid (Grid n ns) = M.set ns 0

-- |Hideously inefficient way of swapping two vectors
swap :: Grid -> Grid -> IO()
swap x@(Grid n xs) (Grid _ ys) = forM_ [0..(vectorLength n - 1)] $ \i -> do
                        xtmp <- GM.unsafeRead xs i
                        ytmp <- GM.unsafeRead ys i
                        GM.unsafeWrite xs i ytmp
                        GM.unsafeWrite ys i xtmp
  

-- |Create an empty vector
emptyGrid :: Int -> IO Grid
emptyGrid sz = do
  d <- GM.unsafeNewWith (vectorLength sz) 0
  return (Grid sz d)

-- |Translate from 2D to 1D co-ordinates
ix :: Int -> (Int,Int) -> Int
ix n (i,j) = i + (n+2) * j where

-- |Look up the given point from the vector
get :: Grid -> (Int,Int) -> IO Double
get (Grid sz g) p = GM.read g (ix sz p)

-- |Add the sources together, writing the content out to x
addSource :: Grid -> Grid -> Double -> IO ()
addSource (Grid sz x) (Grid _ s) dt = forM_ [0..(vectorLength sz - 1)] $ \i -> do
                         xa <- GM.unsafeRead x i
                         sa <- GM.unsafeRead s i 
                         GM.unsafeWrite x i (xa + sa*dt)

-- |Write a single value at the given co-ordinates
writeVal :: Grid -> (Int,Int) -> Double -> IO ()
writeVal (Grid sz d) p = GM.unsafeWrite d (ix sz p) 

-- |Write multiple values
setVals :: Grid -> [((Int,Int),Double)] -> IO ()
setVals g@(Grid sz d) vals = forM_ vals (uncurry (writeVal g))

-- |Read the value at the given point
readVal :: Grid -> (Int,Int) -> IO Double
readVal (Grid sz d) p = GM.unsafeRead d (ix sz p)

-- |This code is vomit inducing, but handles the edge cases..
setBnd :: Int -> Grid -> IO()
setBnd b g@(Grid sz x) = forM_ [1..sz] 
                          (\i -> 
                               do
                                 a1 <- readVal g (1,i)
                                 a2 <- readVal g (sz,i)
                                 a3 <- readVal g (i,1)
                                 a4 <- readVal g (i,sz)
                                 let mx | b == 1 = -1
                                        | otherwise = 1
                                 let my | b==2 = -1
                                        | otherwise = 1
                                 setVals g [((0,i)   ,mx * a1)
                                           ,((sz+1,i),mx * a2)
                                           ,((i,0)   ,my * a3)
                                           ,((i,sz+1),my * a4)])
                         >> do
                           x10 <- readVal g (1,0)
                           x01 <- readVal g (0,1)
                           x1n1 <- readVal g (1,sz+1)
                           x0n <- readVal g (0,sz)
                           xn0 <- readVal g (sz,0)
                           xn11 <- readVal g (sz+1,1)
                           xnn1 <- readVal g (sz,sz+1)
                           x1nn <- readVal g (sz+1,sz)
                           setVals g [((0,0)      ,0.5 * (x10  + x01))
                                     ,((0,sz+1)   ,0.5 * (x1n1 + x0n))
                                     ,((sz+1,0)   ,0.5 * (xn0  + xn11))
                                     ,((sz+1,sz+1),0.5 * (xnn1 + x1nn))]

-- |A simple loop over each pixel
forEachPixel :: Grid -> ((Int,Int) -> IO()) -> IO()
forEachPixel g@(Grid n gs) = forM_ [(u,v) | u<-[1..n], v <- [1..n]]

-- |For simplicity, just consider up,down,left,right to be the neighbours
neighbours :: Grid -> (Int,Int) -> IO (Double,Double,Double,Double)
neighbours g (x,y) = do 
  up <- readVal g (x-1,y)
  down <- readVal g (x+1,y)
  left <- readVal g (x,y-1)
  right <- readVal g (x,y+1)
  return (up,down,left,right)

linSolveStep :: Int -> Grid -> Grid -> Double -> Double -> IO ()
linSolveStep b x x0 a c = forEachPixel x
                            (\(i,j) -> 
                             do
                               (up,down,left,right) <- neighbours x (i,j)
                               x0v <- readVal x0 (i,j)
                               writeVal x (i,j) ((x0v + a*(up + down + left + right)) / c))
                          >> setBnd b x

linSolve :: Int -> Grid -> Grid -> Double -> Double -> IO()
linSolve b x x0 a c = forM_ [1..20] (\_ -> linSolveStep b x x0 a c)

diffuse :: Int -> Grid -> Grid -> Double -> Double -> IO()
diffuse b x@(Grid n _) x0 diff dt = linSolve b x x0 a (1 + 4*a) where
    a = dt * diff * (fromIntegral n * fromIntegral n)

advect :: Int -> Grid -> Grid -> Grid -> Grid -> Double -> IO ()
advect b d@(Grid n _) d0 u v dt = forEachPixel d
                                   (\(i,j) ->
                                    do
                                      uVal <- readVal u (i,j)
                                      vVal <- readVal v (i,j)
                                      let n5 = fromIntegral n + 0.5
                                          x = min n5 (max 0.5 (fromIntegral i - dt0 * uVal))
                                          y = min n5 (max 0.5 (fromIntegral j - dt0 * vVal))
                                          i0 = truncate x
                                          i1 = i0 + 1
                                          j0 = truncate y
                                          j1 = j0 + 1
                                          s1 = x - fromIntegral i0
                                          s0 = 1 - s1
                                          t1 = y - fromIntegral j0
                                          t0 = 1 - t1
                                      xd0 <- readVal d0 (i0,j0)
                                      xd1 <- readVal d0 (i0,j1)
                                      xd2 <- readVal d0 (i1,j0)
                                      xd3 <- readVal d0 (i1,j1)
                                      writeVal d (i,j) (s0*(t0*xd0 + t1*xd1) + s1*(t0*xd2+ t0*xd3)))
                                  >> setBnd b d
                                      where
                                        dt0 = dt * fromIntegral n

project :: Grid -> Grid -> Grid -> Grid -> IO ()
project u@(Grid n _) v p div = forEachPixel u
                                (\(i,j) ->
                                     do
                                       u0 <- readVal u (i+1,j)
                                       u1 <- readVal u (i-1,j)
                                       v0 <- readVal v (i,j+1)
                                       v1 <- readVal v (i,j-1)
                                       writeVal div (i,j) (-0.5 * ((u0-u1+v0-v1) / fromIntegral n))
                                       writeVal p (i,j) 0)
                               >> setBnd 0 div 
                               >> setBnd 0 p 
                               >> linSolve 0 p div 1 4 
                               >> forEachPixel p
                                   (\(i,j) ->
                                    do
                                      (up,down,left,right) <- neighbours p (i,j)
                                      u0 <- readVal u (i,j)
                                      v0 <- readVal v (i,j)
                                      writeVal u (i,j) (u0 - 0.5*fromIntegral n*(down - up))
                                      writeVal v (i,j) (v0 - 0.5*fromIntegral n*(right - left)))
                               >> setBnd 1 u
                               >> setBnd 2 v

densStep :: Grid -> Grid -> Grid -> Grid -> Double -> Double -> IO ()
densStep x@(Grid n _) x0 u v diff dt = do
             addSource x x0 dt
             swap x0 x
             diffuse 0 x x0 diff dt
             swap x0 x
             advect 0 x x0 u v dt

velStep :: Grid -> Grid -> Grid -> Grid -> Double -> Double -> IO ()
velStep u v u0 v0 visc dt = do
             addSource u u0 dt
             addSource v v0 dt
             swap u0 u
             diffuse 1 u u0 visc dt
             swap v0 v
             diffuse 2 v v0 visc dt
             project u v u0 v0
             swap u0 u
             swap v0 v
             advect 1 u u0 u0 v0 dt
             advect 2 v v0 u0 v0 dt
             project u v u0 v0

testSetBnd = do
  putStrLn "Testing setBnd"
  a <- listToVec [0..15]
  let expected = [5,5,6,6,5,5,6,6,9,9,10,10,9,9,10,10]
  let example = Grid 2 a
  setBnd 3 example
  b <- vecToList a
  print (b == expected)  

testLinSolveStep = do
  putStrLn "Testing LinSolveStep"
  x <- listToVec [0..15]
  x0 <- listToVec [0..15]
  let expectedLinStep = [0,-16.25,-27.9375,0,16.25,16.25,27.9375,27.9375,37.6875,37.6875,70.468750,70.468750,0,-37.6875,-70.46875,0]
  linSolveStep 2 (Grid 2 x) (Grid 2 x0) 3 4 
  c <- vecToList x
  print (c == expectedLinStep)

testLinSolve = do
  putStrLn "Testing LinSolve"
  x <- listToVec [0..15]
  x0 <- listToVec [0..15]
  let expected = [54.999996,54.999996,56.749998,56.749998,54.999996,54.999996,56.749998,56.749998,58.250002,58.250002,60.000002,60.000002,58.250002,58.250002,60.000002,60.000002]
  linSolve 0 (Grid 2 x) (Grid 2 x0) 1 4
  c <- vecToList x
  print (nearlyEqual c expected)

testAdvect = do
  putStrLn "Testing advect"
  let expected = [2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5]
  a <- listToVec [0..15]
  b <- listToVec [0..15]
  c <- listToVec [0..15]
  d <- listToVec [0..15]
  advect 3 (Grid 2 a) (Grid 2 b) (Grid 2 c) (Grid 2 d) 9
  result <- vecToList a
  print (nearlyEqual result expected)

testProject = do
  putStrLn "Testing project"
  u <- listToVec [0..15]
  v <- listToVec [0..15]
  p <- listToVec [0..15]
  div <- listToVec [0..15]
  project (Grid 2 u) (Grid 2 v) (Grid 2 p) (Grid 2 div)
  uResult <- vecToList u
  vResult <- vecToList v
  pResult <- vecToList p
  divResult <- vecToList div
  let expectedU = [0.000000,5.416666,6.416666,0.000000,-5.416666,5.416666,6.416666,-6.416666,-9.416666,9.416666,10.416666,-10.416666,0.000000,9.416666,10.416666,0.000000]
      expectedV = [0.000000,-5.416666,-6.416666,0.000000,5.416666,5.416666,6.416666,6.416666,9.416666,9.416666,10.416666,10.416666,0.000000,-9.416666,-10.416666,0.000000]
      expectedP = [-16.180556,-16.180556,-16.597222,-16.597222,-16.180556,-16.180556,-16.597222,-16.597222,-16.597222,-16.597222,-17.013889,-17.013889,-16.597222,-16.597222,-17.013889,-17.013889]
      expectedDiv = [-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000]
  print (nearlyEqual uResult expectedU && nearlyEqual vResult expectedV &&
         nearlyEqual pResult expectedP && nearlyEqual divResult expectedDiv)

testVelStep = do
  putStrLn "VelStep testing"
  let expectedX = [0.000000,0.011987,0.041284,0.000000,-0.011987,0.011987,0.041284,-0.041284,-0.016870,0.016870,0.016870,-0.016870,0.000000,0.016870,0.016870,0.000000]
      expectedY = [0.000000,-0.016870,-0.011987,0.000000,0.016870,0.016870,0.011987,0.011987,0.016870,0.016870,0.021753,0.021753,0.000000,-0.016870,-0.021753,0.000000]
      expectedU = [-0.023750,-0.023750,0.000444,0.000444,-0.023750,-0.023750,0.000444,0.000444,-0.004439,-0.004439,0.014872,0.014872,-0.004439,-0.004439,0.014872,0.014872]
      expectedV = [-0.043505,-0.043505,0.009765,0.009765,-0.043505,-0.043505,0.009765,0.009765,-0.000000,-0.000000,0.033740,0.033740,-0.000000,-0.000000,0.033740,0.033740]
  x <- listToVec [0..15]
  y <- listToVec [0..15]
  u <- listToVec [0..15]
  v <- listToVec [0..15]
  velStep (Grid 2 x) (Grid 2 y) (Grid 2 u) (Grid 2 v) 3 4
  xResult <- vecToList x
  yResult <- vecToList y
  uResult <- vecToList u
  vResult <- vecToList v
  print (nearlyEqual xResult expectedX && nearlyEqual yResult expectedY &&
         nearlyEqual uResult expectedU && nearlyEqual vResult expectedV)

main = defaultMain [
        bgroup "Mutable Fluids" [ 
         bench "LinSolve 10" $ whnf profileLinSolve 10
        ,bench "LinSolve 100" $ whnf profileLinSolve 100
        ,bench "LinSolve 1000" $ whnf profileLinSolve 1000
        ]]

profileLinSolve n = do
  a <- emptyGrid n
  b <- emptyGrid n
  linSolve 0 a b 1 4  
  return ()


{--

main = do
  testSetBnd
  testLinSolveStep
  testLinSolve
  testAdvect
  testProject
  testVelStep
  return ()

--}