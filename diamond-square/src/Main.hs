{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Diagrams.Prelude hiding (Point, origin)
-- import Diagrams.Backend.SVG.CmdLine

import Control.Lens

data Point = Point Int Int deriving (Show,Eq)

data Square = Square
              {
                _topLeft :: Point                
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
move p = topLeft `over` (addPoint p)

divide :: Square -> [Square]
divide sq = [newSq,topRight,bottomLeft,bottomRight]
  where
    topRight = move (Point 0 offset) newSq
    bottomLeft = move (Point offset 0) newSq
    bottomRight = move (Point offset offset) newSq
    newSq = size `over` (`div` 2) $ sq
    offset = sq^.size `div` 2

allSubSquares :: (Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f sq)

main :: IO ()
main = undefined
