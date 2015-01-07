{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Codec.Picture
import qualified Data.Map.Strict as M

import Data.Random.Normal
import Control.Monad (liftM,liftM2)
import Control.Applicative

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
averageHeight eps sq = min 1 $ abs $ eps + ((sq^.tl + sq^.tr + sq^.bl + sq ^.br) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (sq^.tl + sq^.tr) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (sq^.bl + sq^.br) / 2.0 

divide :: Double -> Square -> [Square]
divide eps parent = [
    set tr avgTopHeight $ set br avgHeight    sq
  , set tl avgTopHeight $ set bl avgHeight    (move (offset,0) sq)
  , set tl avgHeight    $ set bl avgBotHeight (move (offset,offset) sq)
  , set tr avgHeight    $ set br avgBotHeight (move (0,offset) sq)
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

epsilon :: Double
epsilon = 0.1

allSubSquaresPlusPerturbation :: (Double -> Square -> [Square]) -> Square -> IO [Square]
allSubSquaresPlusPerturbation f sq
  | isUnit sq = return [sq]
  | otherwise = do
    let sz = logBase 2 (fromIntegral $ sq^.size) :: Double
    x <- normalIO' (0,sz) 
    liftM concat $ mapM (allSubSquaresPlusPerturbation f) (f (x * epsilon) sq)
    

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
  sqs <- allSubSquaresPlusPerturbation divide sq
  let f x y = scale (M.findWithDefault 0 (x,y) pixels)
      pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) sqs
  return (generateImage f imageSize imageSize)

main :: IO ()
main = do
  img <- generatePlasma2 (Square origin imageSize 0.9 0.25 0.75 0.8)
  writePng "/home/jefff/Desktop/random.png" img

