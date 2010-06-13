{-# LANGUAGE CPP, FlexibleInstances #-}

module OrbitTest where

import Orbit
import Test.QuickCheck



instance Arbitrary Object where
    arbitrary = do
      px <- arbitrary
      py <- arbitrary
      m <- arbitrary
      vx <- arbitrary
      vy <- arbitrary
      return Object {
                   position = Vec px py
                 , mass = abs m + 0.1 -- zero mass not supported
                 , velocity = Vec vx vy
                 , force = zero
                 }

energy :: [Object] -> Double
energy os = sum (map ke os) where
    ke o = 0.5 * (mass o) * v * v 
        where
          v = (magnitude $ velocity o)

prop_EnergyConserved :: [Object] -> Bool
prop_EnergyConserved os = abs ((energy os) - (energy $ update os)) < 0.01 where
    update =calculateForcesOnAll . accelerateAll . repositionAll

prop_unitLength :: Double -> Double -> Bool
prop_unitLength 0 0 = True
prop_unitLength x y = abs ((magnitude $ unit v) - 1.0) < 0.0001 where
    v = Vec x y :: Vec Force

