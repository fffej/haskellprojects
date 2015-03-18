module Irrigation where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

type Location = (Int,Int)

data Sprinkler = Sprinkler 
    {
      location :: Location
    , radius :: Int
    } 

data CropField = CropField
    {
      rows :: Int
    , columns :: Int
    , crops :: [Location] 
    }

grid :: CropField -> [Location]
grid c = [(x,y) | x <- [0..rows c], y <- [0..columns c]]

intDistance :: Location -> Location -> Int
intDistance (x1,y1) (x2,y2) = floor (sqrt (dx*dx + dy*dy))
    where
      dx = fromIntegral (x1 - x2)
      dy = fromIntegral (y1 - y2)

bestLocation :: CropField -> Int -> Location
bestLocation field radius = fst $ maximumBy (comparing snd) $ map (location &&& score field) sprinklers
    where
      sprinklers = [Sprinkler loc radius | loc <-  grid field]

score :: CropField -> Sprinkler -> Int
score field sprinkler = killedCrop + length (filter (inRange sprinkler) (crops field))
    where
      killedCrop = if location sprinkler `elem` crops field then (- 1) else 0
 
inRange :: Sprinkler -> Location -> Bool
inRange s p = intDistance l p <= r
    where
      r = radius s
      l = location s
      
