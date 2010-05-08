module ThemePark2 (main) where

import Data.Vector.Unboxed (fromList,unsafeIndex,Vector,length)
import Control.Monad (forM_)

type IntVector = Vector Int

-- Apply a function a fixed number of times
nIterate :: (a -> a) -> a -> Integer -> a
nIterate f a 0 = a
nIterate f a n = nIterate f (f a) (n - 1)

readVec :: IntVector -> Int -> Int
readVec v i = unsafeIndex v i

biggestSum' :: IntVector -> Int -> Integer -> Integer -> Integer -> (Integer,Int)
biggestSum' x offset limit current iterations | x0 + current > limit || (fromInteger iterations) >= v = (current, offset)
                                              | otherwise = biggestSum' x ((offset + 1) `mod` v) limit (current + x0) (iterations + 1) 
    where
      x0 = fromIntegral (readVec x offset) :: Integer
      v = Data.Vector.Unboxed.length x


biggestSum :: Int -> IntVector -> Integer -> (Integer,Int)
biggestSum offset x limit = biggestSum' x offset limit 0 0 

fillCoaster :: Integer -> Int -> IntVector -> (Integer, Int)
fillCoaster capacity offset groups = biggestSum offset groups capacity

fillCoaster' :: Integer -> Int -> IntVector -> (Integer,Int) -> (Integer,Int)
fillCoaster' k n groups (count,offset) = (count + newCount, newOffset) where
    (newCount,newOffset) = fillCoaster k offset groups 

runTest :: Integer -> Integer -> IntVector -> Integer
runTest rideTimes capacity groups = fst $ nIterate f (0,0) rideTimes where
    f = fillCoaster' capacity 0 groups

runTestFromList :: Integer -> Integer -> [Int] -> Integer
runTestFromList a b c = runTest a b (fromList c) 

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read

exampleTest = do
  print (runTestFromList 4 6 [1,4,2,1])
  print (runTestFromList 100 10 [1])
  print (runTestFromList 5 5 [2,4,2,3,4,2,1,2,1,3])

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
              ans = runTest r k (fromList groups)
          putStrLn ("Case #" ++ show t ++ ": " ++ show ans)
          return ()
       )


{-

main = do
  print (runTestFromList 12312 123123 [1..100000])


runTest :: Integer -> Integer -> Int -> IntVector -> Integer
runTest rideTimes capacity numGroups groups = fst $ nIterate f (0,0) rideTimes where
    f = fillCoaster' capacity numGroups groups

exampleTest = do
  print (runTestFromList 4 6 [1,4,2,1])
  print (runTestFromList 100 10 [1])
  print (runTestFromList 5 5 [2,4,2,3,4,2,1,2,1,3])

-}