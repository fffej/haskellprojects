module Snapper where

import Data.Vector.Unboxed as V
import Control.Monad (forM_)
import Data.Int (Int8)

-- 0 ≤ N ≤ 30;
-- 0 ≤ K ≤ 10^8

type IntVector = Vector Int8

statePower :: Bool -> Bool -> Int8
statePower False False = 0
statePower False True = 1
statePower True False = 2
statePower True True = 3

stateInt :: Int8 -> Bool
stateInt x | x == 2 || x == 3 = True
           | otherwise = False

powerInt :: Int8 -> Bool
powerInt x | x ==1 || x == 3 = True
           | otherwise = False


flipStateInt :: Int8 -> Int8
flipStateInt x | (powerInt x) = statePower (not (stateInt x)) (powerInt x)
               | otherwise = x

turnOffInt :: Int8 -> Int8
turnOffInt x = statePower (stateInt x) False

updatePowerInt :: IntVector -> Bool -> IntVector
updatePowerInt v previous | V.null v = empty
                          | previous = V.cons (statePower (stateInt x) True) (updatePowerInt xs (stateInt x))
                          | otherwise = V.cons (statePower (stateInt x) False) (V.map turnOffInt xs)
    where
      x = V.unsafeHead v
      xs = V.unsafeTail v

clickInt :: IntVector -> IntVector
clickInt xs = updatePowerInt (V.map flipStateInt xs) True

snappersInt :: Int -> IntVector
snappersInt n = V.cons (statePower False True) (V.replicate (n-1) (statePower False False))

runTestInt :: Int -> Int -> String
runTestInt n k | x == 3 = "ON"
               | otherwise = "OFF"
    where
      x = (V.unsafeLast ((iterate clickInt (snappersInt n)) !! k))

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  nStr <- readLn :: IO Integer
  Control.Monad.forM_ [1..nStr] 
       (\t ->
            do
              nk <- getLine
              let (n:k:[]) = Prelude.map readInt (words nk)
                  ans = runTestInt n k 
              putStrLn ("Case #" Prelude.++ show t Prelude.++ ": " Prelude.++ ans)
              return ()) 
