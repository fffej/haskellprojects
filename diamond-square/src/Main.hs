{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Codec.Picture
import qualified Data.Map.Strict as M

import System.Random
import Control.Monad (liftM)

type Point = (Int,Int)

data Square = Square
              {
                _position :: Point                
              , _size    :: Int 
              , _tl      :: Double -- Height of top left
              , _tr      :: Double -- top right
              , _bl      :: Double -- bottom left
              , _br      :: Double -- bottom right
              } deriving (Show,Eq)

makeLenses ''Square

isUnit :: Square -> Bool
isUnit sq = sq^.size == 1

origin :: Point
origin = (0,0)

addPoint :: Point -> Point -> Point
addPoint (x,y) (a,b) = (a+x,b+y)

move :: Point -> Square -> Square
move p = position `over` addPoint p

averageHeight :: Double -> Square -> Double
averageHeight eps sq = abs $ eps + ((sq^.tl + sq^.tr + sq^.bl + sq ^.br) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (sq^.tl + sq^.tr) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (sq^.bl + sq^.br) / 2.0 

divide :: Double -> Square -> [Square]
divide eps parent = [
    set tr avgTopHeight $ set br avgHeight    sq
  , set tl avgTopHeight $ set bl avgHeight    (move (0,offset) sq)
  , set tr avgHeight    $ set br avgBotHeight (move (offset,0) sq)
  , set tl avgHeight    $ set bl avgBotHeight (move (offset,offset) sq)
  ]
  where    
    offset = parent^.size `div` 2
    sq = size `over` (`div` 2) $ parent
    avgTopHeight = averageTopHeight parent
    avgHeight = averageHeight eps parent
    avgBotHeight = averageBottomHeight parent
    
allSubSquares :: (Double -> Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f 0 sq)

allSubSquaresWithRandomPerturbation :: (Double -> Square -> [Square]) -> Square -> IO [Square]
allSubSquaresWithRandomPerturbation f sq
  | isUnit sq = return [sq]
  | otherwise = do
    x <- randomRIO (-(0.1),0.1)
    liftM concat $ mapM (allSubSquaresWithRandomPerturbation f) (f x sq)
    

imageSize :: Int
imageSize = 256

scale :: Double -> Pixel16
scale p = truncate (65536 * p)

generatePlasma :: Square -> Image Pixel16
generatePlasma sq = generateImage f imageSize imageSize
  where
    f x y = scale (M.findWithDefault 0 (x,y) pixels) 
    pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) $ allSubSquares divide sq

generatePlasma2 :: Square -> IO (Image Pixel16)
generatePlasma2 sq = do
  sqs <- allSubSquaresWithRandomPerturbation divide sq
  let f x y = scale (M.findWithDefault 0 (x,y) pixels)
      pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) sqs
  return (generateImage f imageSize imageSize)

main :: IO ()
main = do
  img <- generatePlasma2 (Square origin imageSize 0.9 0.25 0.75 0.2)
  writePng "/home/jefff/Desktop/random.png" img

