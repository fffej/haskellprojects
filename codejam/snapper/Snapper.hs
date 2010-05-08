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


flipState (Snapper state power) | power = Snapper (not state) power
                                | otherwise = Snapper state power

flipStateInt x | (powerInt x) = statePower (not (stateInt x)) (powerInt x)
               | otherwise = x

turnOff :: Snapper -> Snapper
turnOff (Snapper s p) = Snapper s False

turnOffInt :: Int -> Int
turnOffInt x = statePower (stateInt x) False

updatePower :: [Snapper] -> Bool -> [Snapper]
updatePower [] _ = []
updatePower (Snapper state power:xs) previous | previous = Snapper state True : updatePower xs state
                                              | otherwise = Snapper state False : map turnOff xs

updatePowerInt :: [Int] -> Bool -> [Int]
updatePowerInt [] _ = []
updatePowerInt (x:xs) previous | previous = statePower (stateInt x) True : updatePowerInt xs (stateInt x)
                               | otherwise = statePower (stateInt x) False : map turnOffInt xs
                                                
click :: [Snapper] -> [Snapper]
click xs = updatePower (map flipState xs) True

clickInt :: [Int] -> [Int]
clickInt xs = updatePowerInt (map flipStateInt xs) True

snappers :: Int -> [Snapper]
snappers n = Snapper False True : replicate (n-1) (Snapper False False)

snappersInt :: Int -> [Int]
snappersInt n = statePower False True : replicate (n-1) (statePower False False)

lightBulb :: [Snapper] -> String
lightBulb xs | power x && state x = "ON"
             | otherwise = "OFF"
    where
      x = last xs

lightBulbInt :: [Int] -> String
lightBulbInt xs | powerInt x && stateInt x = "ON"
                | otherwise = "OFF"
    where
      x = last xs

runTest :: Int -> Int -> String
runTest n k = lightBulb $ last $ take (k+1) $ iterate click (snappers n)

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
