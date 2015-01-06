{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Codec.Picture
import qualified Data.Map.Strict as M

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
averageHeight eps sq = eps + ((sq^.tl + sq^.tr + sq^.bl + sq ^.br) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (sq^.tl + sq^.tr) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (sq^.bl + sq^.br) / 2.0 

divide :: Square -> [Square]
divide parent = [
              set tr (averageTopHeight parent) $ set br (averageHeight 0 parent) sq
            , topRight parent offset
            , bottomLeft parent offset
            , bottomRight parent offset
            ]
  where    
    offset = parent^.size `div` 2
    sq = size `over` (`div` 2) $ parent 


topRight :: Square -> Int -> Square
topRight parent offset = set tl (averageTopHeight parent) $
                         set bl (averageHeight 0 parent) sq
  where
    sq = move (0,offset) (size `over` (`div` 2) $ parent)

bottomLeft :: Square -> Int -> Square
bottomLeft parent offset = set tr (averageHeight 0 parent) $
                           set br (averageBottomHeight parent) sq
  where
    sq = move (offset,0) (size `over` (`div` 2) $ parent)

bottomRight :: Square -> Int -> Square
bottomRight parent offset = set tl (averageHeight 0 parent) $
                            set bl (averageBottomHeight parent) sq
  where
    sq = move (offset,offset) (size `over` (`div` 2) $ parent)

allSubSquares :: (Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f sq)


imageSize :: Int
imageSize = 256

generatePlasma :: Square -> Image Pixel16
generatePlasma sq = generateImage f imageSize imageSize
  where
    f x y = truncate (65536 * M.findWithDefault 0 (x,y) pixels)
    pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) $ allSubSquares divide sq

main :: IO ()
main = do
  writePng "/home/jefff/Desktop/allWhite.png" $ generatePlasma (Square origin imageSize 1 1 1 1)
  writePng "/home/jefff/Desktop/allBlack.png" $ generatePlasma (Square origin imageSize 0 0 0 0)
  writePng "/home/jefff/Desktop/leftToRight.png" $ generatePlasma (Square origin imageSize 1 1 0 0)
  writePng "/home/jefff/Desktop/rightToLeft.png" $ generatePlasma (Square origin imageSize 0 0 1 1)
