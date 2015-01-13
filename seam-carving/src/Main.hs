{-# LANGUAGE NoMonomorphismRestriction #-}
module SeamCarver where

import Codec.Picture
import Codec.Picture.Types

import Data.Either

-- A seam is simply a path of pixels
data Seam = Seam [(Int,Int)]

-- An energy map is just a translation into features
-- where stronger features have stronger values
energyMap :: Image Pixel32 -> Image Pixel32
energyMap = undefined

generateSeams :: Image Pixel32 -> [Seam]
generateSeams = undefined

removeSeams :: Image Pixel32 -> [Seam] -> Image Pixel32
removeSeams = undefined

-- TODO provide some translation mechanism here
toGrayScale :: Pixel a => DynamicImage -> Image a
toGrayScale (ImageY8 x) = undefined
toGrayScale (ImageY16 x) = undefined
toGrayScale (ImageYF x) = undefined
toGrayScale (ImageYA8 x) = undefined
toGrayScale (ImageYA16 x) = undefined
toGrayScale (ImageRGB8 x) = undefined
toGrayScale (ImageRGB16 x) = undefined
toGrayScale (ImageRGBF x) = undefined
toGrayScale (ImageRGBA8 x) = undefined
toGrayScale (ImageRGBA16 x) = undefined
toGrayScale (ImageYCbCr8 x) = undefined
toGrayScale (ImageCMYK8 x) = undefined
toGrayScale (ImageCMYK16 x) = undefined


main :: IO ()
main = do
  putStrLn "Hello world"

