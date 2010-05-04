module Fluid where
-- Inspired by http://www.bestinclass.dk/index.php/2010/03/functional-fluid-dynamics-in-clojure/
-- http://github.com/LauJensen/Fluid-Dynamics/raw/master/fluids.clj
-- Navier Stokes (http://en.wikipedia.org/wiki/Navier–Stokes_equations)

import qualified Data.Vector.Unboxed as V

import Criterion.Main
import Test.HUnit
import Data.List (foldl')

type DVector = V.Vector Double

data Grid = Grid Int DVector deriving (Show,Eq)

-- |Note that we create some padding to try and simplify the handling of edges
emptyBoard :: Int -> Grid
emptyBoard sz = Grid sz (V.fromList (replicate ((sz+2)*(sz+2)) 0))

-- |Is a single dimensional array really quicker?
get :: Grid -> (Int,Int) -> Double
get (Grid n b) p = V.unsafeIndex b (ix n p)

-- |Get the XY given the length of one of the sides
ix :: Int -> (Int,Int) -> Int
ix n (i,j) = i + (n+2) * j where

addSource :: Grid -> Grid -> Double -> Grid
addSource (Grid n x) (Grid _ s) dt = Grid n (V.zipWith (\x' s' -> x' + dt * s') x s )

setBnd :: Int -> Grid -> Grid
setBnd b g@(Grid n x) = Grid n (z V.// corners)
    where
      x'@(Grid _ z) = Grid n (x V.// concat [ [(ix n (0,i),   mx * get g (1,i))
                                               ,(ix n (n+1,i), mx * get g (n,i))
                                               ,(ix n (i,0),   my * get g (i,1)) 
                                               ,(ix n (i,n+1), my * get g (i,n))] | i <- [1..n]])
      mx | b==1 = -1
         | otherwise = 1
      my | b==2 = -1
         | otherwise = 1
      corners = [(ix n (0,0)    , 0.5 * (get x' (1,0)   + get x' (0,1)))
                ,(ix n (0,n+1)  , 0.5 * (get x' (1,n+1) + get x' (0,n)))
                ,(ix n (n+1,0)  , 0.5 * (get x' (n,0)   + get x' (n+1,1)))
                ,(ix n (n+1,n+1), 0.5 * (get x' (n,n+1) + get x' (n+1,n)))]

linSolve' :: Int -> Double -> Double -> Grid -> Grid -> Grid
linSolve' b a c g0@(Grid n _) g@(Grid _ gs) = setBnd b result where
    result = Grid n (V.foldl' fx gs (V.fromList [(i,j) | i <- [1..n], j <- [1..n]]))
    fx us (i,j) = v
        where
          p = ix n (i,j)
          left = ix n (i-1,j)
          right = ix n (i+1,j)
          down = ix n (i,j-1)
          up = ix n (i,j+1)
          v = V.unsafeUpd us [(p, (get g0 (i,j) + a * (V.unsafeIndex us left +
                                                       V.unsafeIndex us right + 
                                                       V.unsafeIndex us down +
                                                       V.unsafeIndex us up)) /c)]
                                                    
linSolve :: Int -> Grid -> Grid -> Double -> Double -> Grid
linSolve b x x0 a c = iterate (linSolve' b a c x0) x !! 20

diffuse :: Int -> Grid -> Grid -> Double -> Double -> Grid
diffuse b x@(Grid n _) x0 diff dt = linSolve b x x0 a (1+4*a) where
    a = dt * diff * fromIntegral (n*n)

advect :: Int -> Grid -> (Grid,Grid) -> Double -> Grid
advect b d0@(Grid n _) (u,v) dt = setBnd b (Grid n (e V.// [(ix n (i,j),adv i j) | i <- [1..n], j <- [1..n]])) where
    dt0 = dt * fromIntegral n
    (Grid _ e) = emptyBoard n
    adv i j = s0*(t0*get d0 (i0,j0) + t1*get d0 (i0,j1)) +
              s1*(t0*get d0 (i1,j0) + t0*get d0 (i1,j1))
        where
          n5 = fromIntegral n + 0.5
          x = min n5 (max 0.5 (fromIntegral i - dt0 * get u (i,j)))
          y = min n5 (max 0.5 (fromIntegral j - dt0 * get v (i,j)))
          i0 = truncate x
          i1 = i0 + 1
          j0 = truncate y
          j1 = j0 + 1
          s1 = x - fromIntegral i0
          s0 = 1 - s1
          t1 = y - fromIntegral j0
          t0 = 1 - t1
    
project :: (Grid,Grid) -> ((Grid,Grid),(Grid,Grid))
project (u@(Grid n _),v) = ((setBnd 1 u',setBnd 2 v'),(p,d))
    where
      d = Fluid.div (u,v)
      (Grid _ e) = emptyBoard n
      p = linSolve 0 (setBnd 0 (emptyBoard n)) d 1 4
      nd = fromIntegral n
      u' = Grid n (e V.// [(ix n (i,j), get u (i,j) - 0.5*nd*(get p (i+1,j) - get p (i-1,j))) | i <- [1..n], j <- [1..n]])
      v' = Grid n (e V.// [(ix n (i,j), get v (i,j) - 0.5*nd*(get p (i,j+1) - get p (i,j-1))) | i <- [1..n], j <- [1..n]])

div :: (Grid,Grid) -> Grid
div (u@(Grid n _),v) = setBnd 0 d 
    where
      (Grid _ e) = emptyBoard n
      d = Grid n (e V.// [(ix n (i,j), 
                           -0.5 * ((get u (i+1,j) - get u (i-1,j) + get v (i,j+1) - get v (i,j-1)) / fromIntegral n))
                          | i <- [1..n], j <- [1..n]])
    
densStep :: Grid -> Grid -> (Grid,Grid) -> Double -> Double -> (Grid,Grid)
densStep x x0 (u,v) diff dt = (advect 0 x'' (u,v) dt,x'')
    where
      x' = addSource x x0 dt
      x'' = diffuse 0 x x' diff dt

velStep :: (Grid,Grid) -> (Grid,Grid) -> Double -> Double -> (Grid,Grid)
velStep (u,v) (u0,v0) dt visc = (u00,v00)
    where
      u' = diffuse 1 u (addSource u u0 dt) visc dt
      v' = diffuse 2 v (addSource v v0 dt) visc dt
      ((u'',v''),(x,y)) = project (u',v') -- u0 and v0 correct
      u''' = advect 1 x (u'',v'') dt
      v''' = advect 2 y (u'',v'') dt
      ((u00,v00), (p,div)) = project (u''',v''')
      

main = defaultMain [
        bgroup "test" [ 
         bench "linsolvestep" $ whnf linsolveStep g
        ,bench "linsolvewhole" $ whnf linsolveT 5
        ]
       ]
    where
      g = emptyBoard 10
      linsolveStep = linSolve' 4 5.0 6.0 
      linsolveT = linSolve 4 g g 4.4

-- Write some tests to compare it against known good output from the C program
testSetBnd = TestCase (assertEqual "for setBnd 3 g" expected actual) where
    expected = Grid 2 (V.fromList [5,5,6,6,5,5,6,6,9,9,10,10,9,9,10,10])
    actual = setBnd 3 (Grid 2 (V.fromList [0..15]))

testLinSolveStep = TestCase (assertEqual "linSolveStep" expected actual) where
    expected = Grid 2 (V.fromList [0,-16.25,-27.9375,0,16.25,16.25,27.9375,27.9375,37.6875,37.6875,70.468750,70.468750,0,-37.6875,-70.46875,0])
    grid = Grid 2 (V.fromList [0..15])
    actual = linSolve' 2 3 4 grid grid

testLinSolveStep3 = TestCase (assertEqual "linsolveStep2" expected actual2) where 
    expected = Grid 2 (V.fromList [0.0,-50.46875,-92.203125,0.0,50.46875,50.46875,92.203125,92.203125,92.953125,92.953125,141.3671875,141.3671875,0.0,-92.953125,-141.3671875,0.0])
    grid = Grid 2 (V.fromList [0..15])
    actual1 = linSolve' 2 3 4 grid grid
    actual2 = linSolve' 2 3 4 grid actual1
      
testAdvect = TestCase (assertEqual "advect" expected actual) where
    actual = advect 3 grid (grid,grid) 9
    grid = Grid 2 (V.fromList [0..15])
    expected = Grid 2 (V.fromList [2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5])

absDifference :: DVector -> DVector -> Double
absDifference v1 v2 = sqrt (V.sum (V.map (\y -> y*y) (V.zipWith (-) v1 v2)))

nearlyEqual :: DVector -> DVector -> Bool
nearlyEqual x y = absDifference x y < 0.0001

testDensStep = TestCase (assertBool "densStep" (nearlyEqual x' x && nearlyEqual x0 x0')) where
    (Grid 2 x',Grid 2 x0') = densStep grid grid (grid,grid) 3 4
    grid = Grid 2 (V.fromList [0..15])
    expected = Grid 2 (V.fromList [0..15])
    x = V.fromList  [11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760,11.495760]
    x0 = V.fromList [11.495760,11.495760,11.636742,11.636742,11.495760,11.495760,11.636742,11.636742,11.791386,11.791386,11.932055,11.932055,11.791386,11.791386,11.932055,11.932055]

testDiv = TestCase (assertEqual "div" actual expected) where
    grid = Grid 2 (V.fromList [0..15])
    expected = Fluid.div (grid,grid)
    actual = Grid 2 (V.fromList [-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5,-2.5])

testLinSolveP = TestCase (assertBool "linSolveP" (nearlyEqual actual expected)) where
    grid = Grid 2 (V.fromList [0..15])
    d = Fluid.div (grid,grid)
    (Grid n actual) = linSolve 0 (setBnd 0 (emptyBoard 2)) d 1 4
    expected = (V.fromList [-16.180556,-16.180556,-16.597222,-16.597222,-16.180556,-16.180556,-16.597222,-16.597222,-16.597222,-16.597222,-17.013889,-17.013889,-16.597222,-16.597222,-17.013889,-17.013889])

testProject = TestCase (assertBool "project" (nearlyEqual u u' && nearlyEqual v v')) where
    grid = Grid 2 (V.fromList [0..15])
    ((Grid _ u',Grid _ v'),_) = project (grid,grid)
    (Grid _ u) = Grid 2 (V.fromList [0.000000,5.416666,6.416666,0.000000,-5.416666,5.416666,6.416666,-6.416666,-9.416666,9.416666,10.416666,-10.416666,0.000000,9.416666,10.416666,0.000000])
    (Grid _ v) = Grid 2 (V.fromList [0.000000,-5.416666,-6.416666,0.000000,5.416666,5.416666,6.416666,6.416666,9.416666,9.416666,10.416666,10.416666,0.000000,-9.416666,-10.416666,0.000000])

{-testVelStep = TestCase (assertBool "velStep" (nearlyEqual actual expected)) where
    grid = Grid 2 (V.fromList [0..15])
    (u,v) = velStep (grid,grid) (grid,grid)-}

tests = TestList [
         TestLabel "setBnd" testSetBnd
        ,TestLabel "linSolveStep" testLinSolveStep
        ,TestLabel "linSolveStep3" testLinSolveStep3
        ,TestLabel "advect" testAdvect
        ,TestLabel "densStep" testDensStep
        ,TestLabel "project" testProject
        ,TestLabel "LinSolveP" testLinSolveP
        ,TestLabel "div" testDiv
        ]