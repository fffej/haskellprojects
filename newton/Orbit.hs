module Orbit where

import Data.List (delete,(\\),nub,nubBy)

-- Phantom types so as to not mix up quantities
data Position = Position deriving Eq
data Velocity = Velocity deriving Eq
data Force = Force deriving Eq

data Vec a = Vec Double Double deriving (Show,Eq)

data Object = Object {
      position :: Vec Position
    , mass :: Double
    , velocity :: Vec Velocity
    , force :: Vec Force
} deriving (Show,Eq)

convert :: Vec a -> b -> Vec b
convert (Vec x y) _ = Vec x y :: Vec b

add :: Vec a -> Vec a -> Vec a
add (Vec u v) (Vec x y) = Vec (u+x) (v+y)

sub :: Vec a -> Vec a -> Vec a
sub (Vec u v) (Vec x y) = Vec (u-x) (v-y)

average :: Vec a -> Vec a -> Vec a
average (Vec u v) (Vec x y) = Vec ((u+x)/2)  ((v+y)/2)

distance :: Vec a -> Vec a -> Double
distance (Vec u v) (Vec x y) = sqrt (a + b) where
    sq z = z*z
    a = sq (u - x)
    b = sq (v - y)

scale :: Vec a -> Double -> Vec a
scale (Vec x y) s = Vec (x*s) (y*s)

magnitude :: Vec a -> Double
magnitude (Vec x y) = sqrt (x*x + y*y)

unit :: Vec a -> Vec a
unit v | mv == 0 = v 
       | otherwise = scale v (1 / mv)
       where
         mv = magnitude v

zero :: Vec a
zero = Vec 0 0

rotate90 :: Vec a -> Vec a
rotate90 (Vec x y) = Vec (- y) x

gravity :: Double -> Double -> Double -> Double
gravity m1 m2 r | r == 0 = 0
                | otherwise = (m1 * m2) / (r * r)

forceBetween :: Object -> Object -> Vec Force
forceBetween (Object p1 m1 _ _)
             (Object p2 m2 _ _) = scale uv g where
                 uv = convert (unit (sub p2 p1)) Force
                 g = gravity m1 m2 (distance p1 p2) 

accumulateForces :: Object -> [Object] -> Object
accumulateForces o os = o {
                          force = foldl forceFunc zero (delete o os) 
                        } 
    where
      forceFunc f target = add f (forceBetween o target)

calculateForcesOnAll :: [Object] -> [Object]
calculateForcesOnAll os = map (`accumulateForces` os) os

accelerate :: Object -> Object
accelerate o = o {
                 force = zero
               , velocity = av
               } 
    where
      f = force o
      m = mass o
      v = velocity o
      av = add v (convert (scale f (1 / m)) Velocity)

accelerateAll :: [Object] -> [Object]
accelerateAll = map accelerate 

reposition :: Object -> Object
reposition o = o {
                 position = add p (convert v Position)
               }
    where
      p = position o
      v = velocity o

repositionAll :: [Object] -> [Object]     
repositionAll = map reposition

collide :: Object -> Object -> Bool
collide x y = distance (position x) (position y) <= 3

merge :: Object -> Object -> Object
merge x y = Object {
              position = add p1 d
            , mass = mergedMass
            , velocity = scale (add mv1 mv2) (1 / mergedMass)
            , force = add (force x) (force y)
            }
    where
      mx = mass x
      my = mass y
      mergedMass = mx + my
      s = mx / mergedMass
      p1 = position x
      p2 = position y
      uv = unit $ sub p2 p1
      d = scale uv s
      mv1 = scale (velocity x) mx
      mv2 = scale (velocity y) my

collideAll :: [Object] -> [Object] 
collideAll os = merged ++ (os \\ collidedObjects)
    where
      pairs = nubBy (\(a,b) (c,d) -> a==d && b==c) [(x,y) | x<-os,y<-os, x/=y] :: [(Object,Object)]
      collidedPairs = filter (uncurry collide) pairs
      collidedObjects = nub $ concatMap (\(x,y) -> [x,y]) collidedPairs
      merged = map (uncurry merge) collidedPairs

updateAll :: [Object] -> [Object]
updateAll = collideAll . calculateForcesOnAll . accelerateAll . repositionAll