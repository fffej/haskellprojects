{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Diagrams.Prelude hiding (Point)
import Diagrams.Backend.SVG.CmdLine

import Control.Lens

data Point = Point Int Int deriving (Show,Eq)

data Square = Square
              {
                _topLeft :: Point                
              , _size    :: Int -- TODO enforce only 2^x
              , _height  :: Int
              } deriving (Show,Eq)

makeLenses ''Square

addPoint :: Point -> Point -> Point
addPoint (Point x y) (Point a b) = Point (a+x) (b+y)

move :: Point -> Square -> Square
move p = topLeft `over` (addPoint p)

divide :: Square -> [Square]
divide sq = [topLeft,topRight,bottomLeft,bottomRight]
  where
    topLeft = undefined -- same top left, size / 2
    topRight = undefined
    bottomLeft = undefined 
    bottomRight = undefined
    newSize = size `over` (`div` 2) $ sq

main :: IO ()
main = undefined
