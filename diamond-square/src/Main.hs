{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

data Point = Point Int Int deriving (Show,Eq)

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
origin = Point 0 0

addPoint :: Point -> Point -> Point
addPoint (Point x y) (Point a b) = Point (a+x) (b+y)

move :: Point -> Square -> Square
move p = position `over` addPoint p

divide :: Square -> [Square]
divide sq = [
              topLeft sq 
            , topRight sq offset
            , bottomLeft sq offset
            , bottomRight sq offset
            ]
  where    
    offset = sq^.size `div` 2

averageHeight :: Square -> Double
averageHeight sq = (sq^.tl + sq^.tr + sq^.bl + sq ^.br) / 4.0

averageTopHeight :: Square -> Double
averageTopHeight sq = (sq^.tl + sq^.tr) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (sq^.bl + sq^.br) / 2.0 

topLeft :: Square -> Square
topLeft parent = set tr (averageTopHeight parent) $
                 set br (averageHeight parent) sq
  where
    sq = size `over` (`div` 2) $ parent

topRight :: Square -> Int -> Square
topRight parent offset = set tl (averageTopHeight parent) $
                         set bl (averageHeight parent) sq
  where
    sq = move (Point 0 offset) (size `over` (`div` 2) $ parent)

bottomLeft :: Square -> Int -> Square
bottomLeft parent offset = set tr (averageHeight parent) $
                           set br (averageBottomHeight parent) sq
  where
    sq = move (Point offset 0) (size `over` (`div` 2) $ parent)

bottomRight :: Square -> Int -> Square
bottomRight parent offset = set tl (averageHeight parent) $
                            set bl (averageBottomHeight parent) sq
  where
    sq = move (Point offset offset) (size `over` (`div` 2) $ parent)

allSubSquares :: (Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f sq)

main :: IO ()
main = undefined
