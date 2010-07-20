{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Kepler.Internal where

import CSH2
import Foreign.C.Types
import Foreign.C.Ptr
import System.IO.Unsafe
import Foreign.C

#include "kepler.h"

{#enum solar_system_planets as Planet deriving (Show,Eq) #}

data DegMinSec = DegMinSec {
      degrees :: Int
    , minutes :: Int
    , seconds :: Double
}

{#pointer degminsec as DegMinSecPtr -> DegMinSec#}

reduceAngle :: Double -> Double -> Double
reduceAngle = {#call pure reduceAngle as "reduce_angle"#}