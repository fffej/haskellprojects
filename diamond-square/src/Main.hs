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

move :: Square -> Point -> Square
move sq (x,y) = sq { position = (a+x,b+y) }
  where
    (a,b) = position sq

averageHeight :: Double -> Square -> Double
averageHeight eps sq = eps + ((tl sq + tr sq + bl sq + br sq) / 4.0)

averageTopHeight :: Square -> Double
averageTopHeight sq = (tl sq + tr sq) / 2.0 

averageBottomHeight :: Square -> Double
averageBottomHeight sq = (bl sq + br sq) / 2.0

averageLeftHeight :: Square -> Double
averageLeftHeight sq = (tl sq + bl sq) / 2.0

averageRightHeight :: Square -> Double
averageRightHeight sq = (tr sq + br sq) / 2.0

divide :: Double -> Square -> [Square]
divide eps parent = [
    sq                    { tr = at, br = ah, bl = al } -- top left unchanged
  , (move sq (half,0))    { tl = at, bl = ah, br = ar } -- top right unchanged
  , (move sq (0,half))    { tr = ah, br = ab, tl = al } -- bottom left unchanged
  , (move sq (half,half)) { tl = ah, bl = ab, tr = ar } -- bottom right unchanged
  ]
  where    
    half = size parent `div` 2
    sq = parent { size = half }
    at = averageTopHeight parent
    ah = averageHeight eps parent -- height of middle
    ab = averageBottomHeight parent
    ar = averageRightHeight parent
    al = averageLeftHeight parent
    
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
    zeroToOne = (p - mn) / (mx - mn)

jetMap :: Double -> Double -> Double -> PixelRGB8
jetMap mn mx p = PixelRGB8 (trunc r) (trunc g) (trunc b)
  where
    trunc c = truncate (c * 255)
    (r,g,b) = color $ (p - mn) / (mx -mn)

-- v is bound between 0 and 1
-- dv is 1
color :: Double -> (Double,Double,Double)
color v
  | v < 0.25  = (0,4*v,1)
  | v < 0.50  = (0,1,1 + 4 * (0.25 - v))
  | v < 0.75  = (4 * (v - 0.5),1,0)
  | otherwise = (1,1 + 4 * (0.75 - v),0)


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
  img <- generatePlasma2 jetMap sq
  let img2 = generatePlasma jetMap sq
  writePng "/home/jefff/Desktop/randomC.png" img
  writePng "/home/jefff/Desktop/notrandomC.png" img2

