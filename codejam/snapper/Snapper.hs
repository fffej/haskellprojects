module Snapper (main) where

import Control.Monad (forM_)

data Snapper = Snapper {
      state :: Bool
    , power :: Bool
} deriving (Show,Eq)

-- 0 ≤ N ≤ 30;
-- 0 ≤ K ≤ 10^8

statePower :: Bool -> Bool -> Int
statePower False False = 0
statePower False True = 1
statePower True False = 2
statePower True True = 3

stateInt :: Int -> Bool
stateInt x | x == 2 || x == 3 = True
         | otherwise = False

powerInt :: Int -> Bool
powerInt x | x ==1 || x == 3 = True
         | otherwise = False


flipStateInt x | (powerInt x) = statePower (not (stateInt x)) (powerInt x)
               | otherwise = x

turnOffInt :: Int -> Int
turnOffInt x = statePower (stateInt x) False

updatePowerInt :: [Int] -> Bool -> [Int]
updatePowerInt [] _ = []
updatePowerInt (x:xs) previous | previous = statePower (stateInt x) True : updatePowerInt xs (stateInt x)
                               | otherwise = statePower (stateInt x) False : map turnOffInt xs
                                                
clickInt :: [Int] -> [Int]
clickInt xs = updatePowerInt (map flipStateInt xs) True

snappersInt :: Int -> [Int]
snappersInt n = statePower False True : replicate (n-1) (statePower False False)

lightBulbInt :: [Int] -> String
lightBulbInt xs | powerInt x && stateInt x = "ON"
                | otherwise = "OFF"
    where
      x = last xs

runTestInt :: Int -> Int -> String
runTestInt n k = lightBulbInt $ last $ take (k+1) $ iterate clickInt (snappersInt n)

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  nStr <- readLn :: IO Integer
  forM_ [1..nStr] 
       (\t ->
            do
              nk <- getLine
              let (n:k:[]) = map readInt (words nk)
                  ans = runTestInt n k 
              putStrLn ("Case #" ++ show t ++ ": " ++ ans)
              return ())
