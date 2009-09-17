import Test.HUnit

import Test.QuickCheck

foo :: (Num a) => a -> a -> a -> a
foo a b c = a * b + c

test1 = TestCase (assertEqual "* has higher precedence" 26 (foo 2 10 6))

tests = TestList [TestLabel "Foo test" test1]

addNum :: (Num a) => a -> a -> a
addNum a b = a + b

invariantAddNum a b = (addNum a b) >= b && (addNum a b) >= a

data Point = Point {x :: Float , y :: Float} deriving Show

square :: (Num a) => a -> a
square x = x * x

distance :: Point -> Point -> Float
distance p1 p2 = sqrt(square ((x p1)-(x p2)) + square ((y p1)-(y p2)))

prop_distance ::Point -> Point -> Float -> Float -> Bool
prop_distance p1 p2 d1 d2 = 0.001 > abs (distance p1 p2 - 
                                         distance (Point ((x p1) + d1) ((y p1) + d2))
                                                  (Point ((x p2) + d1) ((y p2) + d2)))

instance Arbitrary Point where
    arbitrary = do
      x <- choose(1,1000) :: Gen Float
      y <- choose(1,1000) :: Gen Float
      return (Point x y)

groupN :: [a] -> Int -> [[a]]
groupN [] _ = []
groupN xs n = a : groupN b n where
              (a,b) = splitAt n xs
