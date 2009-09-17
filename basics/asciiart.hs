-- Quick Program to generate some ASCII art based on PPM files

import Graphics.Pgm

import Text.Parsec.Error
import Data.Array.Base

import Data.List.Split

brightness =  " .`-_':,;^=+/\"|)\\<>)iv%xclrs{*}I?!][1taeo7zjLu" ++ 
              "nT#JCwfy325Fp6mqSghVd4EgXPGZbYkOA&8U$@KHDBWNMR0Q";

-- | Load the first image from the specified PGM file
loadImage :: String -> IO (UArray (Int,Int) Int)
loadImage path = do
                    r <- pgmsFromFile path
                    case r of
                      Left e -> error "Failed to parse file"
                      Right i -> return (head i)

brightnessToChar :: Int -> Int -> Char
brightnessToChar m b = brightness !! 
                       (round ((fromIntegral b) / (fromIntegral m) * (fromIntegral ((length brightness) - 1))))

imageToAscii :: UArray (Int,Int) Int -> UArray (Int,Int) Char
imageToAscii image = amap (brightnessToChar 255)  image
     
convertImage :: String -> String -> IO ()
convertImage image out = do
                       img <- loadImage image
                       let ((_,_),(h,w)) = bounds img
                       let x = imageToAscii img
                       writeFile "/home/jfoster/Desktop/jeff.txt" ([ x ! (i,j) | j <- [0..w], i <- [0..h]])
                       writeFile out (unlines [ [ x ! (i,j) | i <- [0..w] ] | j <- [0..h] ])
                       return ()

    



