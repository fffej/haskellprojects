{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Codec.Picture
import qualified Data.Map.Strict as M

import System.Random
import Control.Monad (liftM)
import Control.Arrow ((&&&))
type Point = (Int,Int)

data Square = Square
              {
                position :: Point                
              , size    :: Int 
              , tl      :: Double -- Height of top left
              , tr      :: Double -- Height of top right
              , bl      :: Double -- Height of bottom left
              , br      :: Double -- Height of bottom right
              } deriving (Show,Eq)

mkSquare :: Int -> IO Square
mkSquare sz = do
  a <- randomRIO(- 0.5, 0.5)
  b <- randomRIO(- 0.5, 0.5)
  c <- randomRIO(- 0.5, 0.5)
  d <- randomRIO(- 0.5, 0.5)
  return (Square (0,0) sz a b c d)

isUnit :: Square -> Bool
isUnit sq = size sq == 1

move :: Point -> Square -> Square
move (x,y) sq = sq { position = (a+x,b+y) }
  where
    (a,b) = position sq

averageHeight :: Double -> Square -> Double
averageHeight eps sq = eps + ((tl sq + tr sq + bl sq + br sq) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (tl sq + tr sq) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (bl sq + br sq) / 2.0

averageLhsHeight :: Square -> Double
averageLhsHeight sq = (tl sq + bl sq) / 2.0

averageRhsHeight :: Square -> Double
averageRhsHeight sq = (tr sq + br sq) / 2.0

divide :: Double -> Square -> [Square]
divide eps parent = [
    sq                        { tr = at, br = ah, bl = al } -- top left unchanged
  , (move (offset,0) sq)      { tl = at, bl = ah, br = ar } -- top right unchanged
  , (move (0,offset) sq)      { tr = ah, br = ab, tl = al } -- bottom left unchanged
  , (move (offset,offset) sq) { tl = ah, bl = ab, tr = ar } -- bottom right unchanged
  ]
  where    
    offset = size parent `div` 2
    sq = parent { size = offset }
    at = averageTopHeight parent
    ah = averageHeight eps parent -- height of diamond
    ab = averageBottomHeight parent
    ar = averageRhsHeight parent
    al = averageLhsHeight parent
    
allSubSquares :: (Double -> Square -> [Square]) -> Square -> [Square]
allSubSquares f sq 
  | isUnit sq = [sq]
  | otherwise = concatMap (allSubSquares f) (f 0 sq)

allSubSquaresPlusPerturbation :: (Double -> Square -> [Square]) -> Square -> IO [Square]
allSubSquaresPlusPerturbation f sq
  | isUnit sq = return [sq]
  | otherwise = do
    let sz = sqrt $ fromIntegral (size sq)
    x <- randomRIO (- 0.5,0.5)
    liftM concat $ mapM (allSubSquaresPlusPerturbation f) (f (sz * x) sq)
    

imageSize :: Int
imageSize = 512

grayScale :: Double -> Double -> Double -> Pixel16
grayScale mn mx p = truncate $ 65535 * zeroToOne
  where
    zeroToOne = ((p - mn) / (mx - mn))

generatePlasma :: Pixel a => (Double -> Double -> Double -> a) -> Square -> Image a
generatePlasma pixFunc sq = generateImage f imageSize imageSize
  where
    minP = maximum $ M.elems pixels
    maxP = minimum $ M.elems pixels
    f x y = pixFunc minP maxP (M.findWithDefault 0 (x,y) pixels) 
    pixels = M.fromList $ map (position &&& averageHeight 0) $ allSubSquares divide sq

generatePlasma2 :: Pixel a => (Double -> Double -> Double -> a) -> Square -> IO (Image a)
generatePlasma2 pixFunc sq = do
  sqs <- allSubSquaresPlusPerturbation divide sq
  let f x y = pixFunc minP maxP (M.findWithDefault 0 (x,y) pixels)
      pixels = M.fromList $ map (position &&& averageHeight 0) sqs
      minP = maximum $ M.elems pixels
      maxP = minimum $ M.elems pixels
  return (generateImage f imageSize imageSize)

main :: IO ()
main = do
  sq <- mkSquare imageSize
  img <- generatePlasma2 grayScale sq
  let img2 = generatePlasma grayScale sq
  writePng "/home/jefff/Desktop/random.png" img
  writePng "/home/jefff/Desktop/notrandom.png" img2

