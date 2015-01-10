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
averageHeight eps sq = eps + ((sq^.tl + sq^.tr + sq^.bl + sq ^.br) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (sq^.tl + sq^.tr) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (sq^.bl + sq^.br) / 2.0

averageLhsHeight :: Square -> Double
averageLhsHeight sq = (sq^.tl + sq^.bl) / 2.0

averageRhsHeight :: Square -> Double
averageRhsHeight sq = (sq^.tr + sq^.br) / 2.0

divide :: Double -> Square -> [Square]
divide eps parent = [
    set tr avgTopHeight $ set br avgHeight    $ set bl avgLhsHeight sq -- top left unchanged
  , set tl avgTopHeight $ set bl avgHeight    $ set br avgRhsHeight (move (offset,0) sq) -- top right unchanged
  , set tr avgHeight    $ set br avgBotHeight $ set tl avgLhsHeight (move (0,offset) sq) -- bottom left unchanged
  , set tl avgHeight    $ set bl avgBotHeight $ set tr avgRhsHeight (move (offset,offset) sq)    -- bottom right unchanged
  ]
  where    
    offset = parent^.size `div` 2
    sq = size `over` (`div` 2) $ parent
    avgTopHeight = averageTopHeight parent
    avgHeight = averageHeight eps parent
    avgBotHeight = averageBottomHeight parent
    avgRhsHeight = averageRhsHeight parent
    avgLhsHeight = averageLhsHeight parent
    
allSubSquares :: (Double -> Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f 0 sq)

allSubSquaresPlusPerturbation :: (Double -> Square -> [Square]) -> Square -> IO [Square]
allSubSquaresPlusPerturbation f sq
  | isUnit sq = return [sq]
  | otherwise = do
    let sz = fromIntegral (sq^.size) :: Double
    x <- randomRIO (- 0.5,0.5)
    liftM concat $ mapM (allSubSquaresPlusPerturbation f) (f (sqrt sz * x) sq)
    

imageSize :: Int
imageSize = 512

scale :: Double -> Double -> Double -> Pixel16
scale mn mx p = truncate $ 65535 * ((p - mn) / (mx - mn))

generatePlasma :: Square -> Image Pixel16
generatePlasma sq = generateImage f imageSize imageSize
  where
    minP = maximum $ M.elems pixels
    maxP = minimum $ M.elems pixels
    f x y = scale minP maxP (M.findWithDefault 0 (x,y) pixels) 
    pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) $ allSubSquares divide sq

generatePlasma2 :: Square -> IO (Image Pixel16)
generatePlasma2 sq = do
  sqs <- allSubSquaresPlusPerturbation divide sq
  let f x y = scale minP maxP (M.findWithDefault 0 (x,y) pixels)
      pixels = M.fromList $ map (\x -> (x^.position, averageHeight 0 x)) sqs
      minP = maximum $ M.elems pixels
      maxP = minimum $ M.elems pixels

  return (generateImage f imageSize imageSize)

main :: IO ()
main = do
  sq <- mkSquare imageSize
  img <- generatePlasma2 sq
  writePng "/home/jefff/Desktop/random.png" img

mkSquare :: Int -> IO Square
mkSquare sz = do
  a <- randomRIO(- 0.5, 0.5)
  b <- randomRIO(- 0.5, 0.5)
  c <- randomRIO(- 0.5, 0.5)
  d <- randomRIO(- 0.5, 0.5)
  return (Square origin sz a b c d)
