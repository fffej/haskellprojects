module Snapper (main) where

import Control.Monad (forM_)

data Snapper = Snapper {
      state :: Bool
    , power :: Bool
} deriving (Show,Eq)

-- 0 ≤ N ≤ 30;
-- 0 ≤ K ≤ 10^8

flipState (Snapper state power) | power = Snapper (not state) power
                                | otherwise = Snapper state power

turnOff :: Snapper -> Snapper
turnOff (Snapper s p) = Snapper s False

updatePower :: [Snapper] -> Bool -> [Snapper]
updatePower [] _ = []
updatePower (Snapper state power:xs) previous | previous = Snapper state True : updatePower xs state
                                              | otherwise = Snapper state False : map turnOff xs
                                                


click :: [Snapper] -> [Snapper]
click xs = updatePower (map flipState xs) True

snappers :: Int -> [Snapper]
snappers n = Snapper False True : replicate (n-1) (Snapper False False)

lightBulb :: [Snapper] -> String
lightBulb xs | power x && state x = "ON"
             | otherwise = "OFF"
    where
      x = last xs

runTest :: Int -> Int -> String
runTest n k = lightBulb $ last $ take (k+1) $ iterate click (snappers n)

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
                  ans = runTest n k 
              putStrLn ("Case #" ++ show t ++ ": " ++ ans)
              return ())
