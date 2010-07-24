{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Kepler.Internal where

{--

# Run C2hs with the appropriate include
  c2hs --cppopts=-I./kepler/src/ -i./kepler/src/ Kepler.chs 

# After building you can start this
  ghci -L./kepler/src/ -lkepler

#Don't forget you can do :!<XYZ> to execute a command from ghci
 :!cshs...
--}

#include "kepler.h"
#include "fund_args.h"
#include "mpc_file.h"

#include "aberration.h"
#include "coordinates.h"
#include "julian_date.h"

-- import CSH2
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Foreign.C

{#context lib="kepler" #}

{#enum solar_system_planets as Planet {}#}
{#enum fund_argument as FundArgument {}#}
{#enum mpc_body_types as MpcBodyType {}#}

data RectangularCoordinates = RectangularCoordinates {
      x :: Double
    , y :: Double
    , z :: Double
}

data EquatorialCoordinates = EquatorialCoordinates {
      rightAscension :: Double
    , declination :: Double
}

data EclipticCoordinates = EclipticCoordinates {
      longitude :: Double -- longitude in radians
    , latitude :: Double -- latitude in radians
}

data JulianDate = JulianDate {
      date1 :: Double
    , date2 :: Double
}

data DegMinSec = DegMinSec {
      degrees :: Int
    , minutes :: Int
    , seconds :: Double
}

{#pointer *rectangular_coordinates as RectangularCoordinatesPtr -> RectangularCoordinates#}
{#pointer *equatorial_coordinates as EquatorialCoordinatesPtr -> EquatorialCoordinates#}
{#pointer *ecliptic_coordinates as EclipticCoordinatesPtr -> EclipticCoordinates#}
{#pointer *julian_date as JulianDatePtr -> JulianDate#}
{#pointer *deg_min_sec as DegMinSecPtr -> DegMinSec#}

rectangular_coordinates_x = {#get rectangular_coordinates.x#}
rectangular_coordinates_y = {#get rectangular_coordinates.y#}
rectangular_coordinates_z = {#get rectangular_coordinates.z#}

equatorial_coordinates_right_ascension = {#get equatorial_coordinates.right_ascension#}
equatorial_coordinates_declination = {#get equatorial_coordinates.declination#}

ecliptic_coordinates_longitude = {#get ecliptic_coordinates.longitude#}
ecliptic_coordinates_latitude = {#get ecliptic_coordinates.latitude#}

instance Storable RectangularCoordinates where
    sizeOf _ = {#sizeof rectangular_coordinates#}

{-
instance Storable RectangularCoordinates where
    sizeOf _ = {#sizeof *rectangular_coordinates #}
    alignment _ = alignment p


_degrees v = {#get deg_min_sec.degrees#}
_minutes v = {#get deg_min_sec.minutes#}
_seconds v = {#get deg_min_sec.seconds#}

-}

--aberrationEarthVelocity :: JulianDate -> RectangularCoordinates
--aberrationEarthVelocity = undefined
{-aberrationEarthVelocity :: IO Int 
aberrationEarthVelocity = alloca f
    where
      f :: JulianDatePtr -> IO Int
      f x = return 1-}
                           
  
                             
                
                