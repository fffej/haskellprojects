module Diamond where

import Data.Array

data Grid = Grid
            {
              size :: Int
            , elements :: Array (Int,Int) Int
            } deriving (Show)

-- TODO add random height
createGrid :: Int -> Grid
createGrid n = Grid n a
  where
    d = 2^n - 1
    b = ((0,0),(d,d)) 
    a = array b [((x,y), 0) | x <- [0..d], y <- [0..d]]

-- Set a particular block to a value
setBlock :: Grid -> ((Int,Int) -> Bool) -> Int -> Grid
setBlock g f v = g {
                     elements = elements g // [(z, v) | z <- indices (elements g), f z]
                   }

render :: Grid -> Int -> Grid
render g r = undefined

