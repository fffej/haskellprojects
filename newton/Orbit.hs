module Orbit where

import Data.List (delete)

-- Phantom types so as to not mix up quantities
data Position = Position deriving Eq
data Velocity = Velocity deriving Eq
data Force = Force deriving Eq

data Vec a = Vec Double Double deriving Eq

data Object = Object {
      position :: Vec Position
    , mass :: Double
    , velocity :: Vec Velocity
    , force :: Vec Force
    , name :: String
} deriving Eq

origin :: Vec a -> Bool
origin (Vec 0 0) = True
origin _ = False

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
unit v = scale v (1 / magnitude v)

zero :: Vec a
zero = Vec 0 0

gravity :: Double -> Double -> Double -> Double
gravity m1 m2 r = (m1 * m2) / (r * r)

forceBetween :: Object -> Object -> Vec Force
forceBetween (Object p1 m1 _ _ _)
             (Object p2 m2 _ _ _) = scale uv g where
                 uv = convert (unit (sub p2 p1)) Force
                 g = gravity m1 m2 (distance p1 p2) 

accumulateForces :: Object -> [Object] -> Object
accumulateForces o os = o {
                          force = foldl forceFunc zero (delete o os) 
                        } 
    where
      forceFunc f target = add f (forceBetween o target)

calculateForcesOnAll :: [Object] -> [Object]
calculateForcesOnAll os = map (\x -> accumulateForces x os) os

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

collided :: Object -> Object -> Bool
collided x y = distance (position x) (position y) <= 3

rotate90 :: Vec a -> Vec a
rotate90 = undefined

merge :: Object -> Object -> Object
merge x y = Object {
              position = average (position x) (position y)
            , mass = mergedMass
            , velocity = scale (add mv1 mv2) (1 / mergedMass)
            , force = add (force x) (force y)
            , name = (name x) ++ "." ++ (name y)
            }
    where
      mx = mass x
      my = mass y
      mergedMass = mx + my
      mv1 = scale (velocity x) mx
      mv2 = scale (velocity y) my
      v = scale (add mv1 mv2) (1 / mergedMass)

collideAll :: [Object] -> [Object]
collideAll os = os
      
updateAll :: [Object] -> [Object]
updateAll = collideAll . calculateForcesOnAll . accelerateAll . repositionAll