module ThemePark (main) where

import Data.List
import Control.Monad (forM_)
-- Roller coaster can hold K people
-- Roller coaster runs R times in a day
-- Queue of people that will only ride in groups

biggestSum' :: [Integer] -> Integer -> Integer -> Integer -> (Integer,Integer)
biggestSum' [] limit current iterations = (current,iterations)
biggestSum' (x:xs) limit current iterations | x + current > limit = (current, iterations)
                                            | otherwise = biggestSum' xs limit (current + x) (iterations + 1)

biggestSum :: [Integer] -> Integer -> (Integer,Integer)
biggestSum x limit = biggestSum' x limit 0 0 


fillCoaster :: Integer -> [Integer] -> (Integer,[Integer])
fillCoaster capacity groups = (size, remaining) where
    (summed,size) = biggestSum groups capacity
    numGroups = length groups
    remaining = take numGroups (drop (fromIntegral summed :: Int) (cycle groups))

nIterate :: (a -> a) -> a -> Integer -> a
nIterate f a 0 = a
nIterate f a n = nIterate f (f a) (n - 1)

fillCoaster' :: Integer -> (Integer,[Integer]) -> (Integer,[Integer])
fillCoaster' k (count,groups) = (count + newCount, g) where
    (newCount,g) = fillCoaster k groups

runTest :: Integer -> Integer -> [Integer] -> Integer
runTest rideTimes capacity groups = fst $ nIterate f (0,groups) rideTimes where
    f = fillCoaster' capacity

{-
3
4 6 4
1 4 2 1
100 10 1
1
5 5 10
2 4 2 3 4 2 1 2 1 3
-}

readInt :: String -> Integer
readInt = read

main :: IO ()
main = do
--  print (runTest2 34523523 2352341 [1..10000])
  print (runTest 34523523 2352341 [1..10000])
  return ()

{-
main = do
  nStr <- readLn :: IO Integer
  forM_ [1..nStr] 
       (\t -> 
        do
          rkn <- getLine
          let (r:k:n:[]) = map readInt (words rkn)
          groupsLine <- getLine
          let groups = map readInt (words groupsLine)
              ans = runTest3 r k groups
          putStrLn ("Case #" ++ show t ++ ": " ++ show ans)
          return ()
       )

-}

exampleTest = do
  print (runTest 4 6 [1,4,2,1])
  print (runTest 100 10 [1])
  print (runTest 5 5 [2,4,2,3,4,2,1,2,1,3])
