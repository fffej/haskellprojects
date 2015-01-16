{-# LANGUAGE NoMonomorphismRestriction #-}
module SeamCarver where

import Codec.Picture
import Codec.Picture.Types

import Data.Either

-- A seam is simply a path of pixels
data Seam = Seam [(Int,Int)]

-- An energy map is just a translation into features
-- where stronger features have stronger values
energyMap :: DynamicImage -> DynamicImage
energyMap = dynamicPixelMap toEnergy

toEnergy :: Pixel a => Image a -> Image a
toEnergy img = generateImage (\x y -> pixelAt img x y) w h
  where
    w = imageWidth img
    h = imageHeight img
    n x y = [pixelAt img xw yw | xw <- [max 0 (x-1) .. x+1], yw <- [min 0 (y-1)..y+1]]

generateSeams :: Image Pixel32 -> [Seam]
generateSeams = undefined

removeSeams :: Image Pixel32 -> [Seam] -> Image Pixel32
removeSeams = undefined

main :: IO ()
main = do
  putStrLn "Hello world"

