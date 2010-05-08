module ThemePark (main) where

import Data.List
import Control.Monad (forM_)
-- Roller coaster can hold K people
-- Roller coaster runs R times in a day
-- Queue of people that will only ride in groups

biggestSum' :: [Int] -> Integer -> Integer -> Integer -> (Integer,Integer)
biggestSum' [] limit current iterations = (current,iterations)
biggestSum' (x:xs) limit current iterations | fromIntegral x + current > limit = (current, iterations)
                                            | otherwise = biggestSum' xs limit (current + fromIntegral x) (iterations + 1)

biggestSum :: [Int] -> Integer -> (Integer,Integer)
biggestSum x limit = biggestSum' x limit 0 0 


fillCoaster :: Integer -> Int -> [Int] -> (Integer,[Int])
fillCoaster capacity numGroups groups = (summed, remaining) where
    (summed,size) = biggestSum groups capacity
    remaining = take numGroups (drop (fromIntegral size :: Int) (cycle groups))

nIterate :: (a -> a) -> a -> Integer -> a
nIterate f a 0 = a
nIterate f a n = nIterate f (f a) (n - 1)

fillCoaster' :: Integer -> Int -> (Integer,[Int]) -> (Integer,[Int])
fillCoaster' k n (count,groups) = (count + newCount, g) where
    (newCount,g) = fillCoaster k n groups

runTest :: Integer -> Integer -> Int -> [Int] -> Integer
runTest rideTimes capacity numGroups groups = fst $ nIterate f (0,groups) rideTimes where
    f = fillCoaster' capacity numGroups

{-
3
4 6 4
1 4 2 1
100 10 1
1
5 5 10
2 4 2 3 4 2 1 2 1 3
-}

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read
{-
main :: IO ()
main = do
  nStr <- readLn :: IO Integer
  forM_ [1..nStr] 
       (\t -> 
        do
          rkn <- getLine
          let (r:k:n:[]) = map readInteger (words rkn)
          groupsLine <- getLine
          let groups = map readInt (words groupsLine)
              ans = runTest r k (fromIntegral n :: Int) groups
          putStrLn ("Case #" ++ show t ++ ": " ++ show ans)
          return ()
       )
-}

main = do
  print (runTest 12312 123123 1000 [1..10000])

exampleTest = do
  print (runTest 4 6 4 [1,4,2,1])
  print (runTest 100 10 1 [1])
  print (runTest 5 5 10 [2,4,2,3,4,2,1,2,1,3])
