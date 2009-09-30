import Data.List

-- 1. Write a function that computes the number of elements in a list.  
-- To test it, ensure it gives the same answers as the standard
-- length function

-- 2. Add a type signature
myLength :: [a] -> Integer
myLength = foldl (\ acc _ -> succ acc) 0 

-- 3. Write a function that computes the mean of a list (i.e. the sum of all 
-- elements in the list divided by its length.
mean :: (Fractional a) => [a] -> a
mean [] = 0
mean x = foldr (+) 0 x / fromIntegral (length x)

-- 4. Turn a list into a plaindrome
palindrome :: [a] -> [a]
palindrome x = x ++ reverse x

-- 5. Write a function that determines whether its nput list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 6. Create a function that sorts a list of lists based on the length of 
-- each sublist.
sortBySubListLength :: [[a]] -> [[a]]
sortBySubListLength = sortBy (\x y -> compare (length x) (length y))

-- 7. Define a function that joins a list of lists together using a separator value
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse s (x:xs) = x : s : myIntersperse s xs


-- 8. Define a function to calculate the height of the tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: (Tree a) -> Integer
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)

-- 9. Consider three two-dimensional points, a,b, and c.  If we look at the angle formed
-- by the line segments from a to b it is either left, right or straight.

-- Left is an existing function...
data Direction = Straight
               | LeftTurn 
               | RightTurn
                 deriving (Show,Eq)

-- 10. Write a function that calculates the turn made by three two-dimensional points
-- and returns a direction
data Point = Point Double Double 
             deriving (Show,Eq)

-- From Wikipedia
-- ... determining whether three points constitute a "left turn" or a "right turn" does 
-- not require computing the actual angle between the two line segments, and can actually 
-- be achieved with simple arithmetic only. For three points (x1,y1), (x2,y2) and (x3,y3), 
-- simply compute the direction of the cross product of the two vectors defined by points 
-- (x1,y1), (x2,y2) and (x1,y1), (x3,y3), characterized by the sign of the expression 
-- (x2 − x1)(y3 − y1) − (y2 − y1)(x3 − x1)
turn :: Point -> Point -> Point -> Direction
turn a b c = makeDirection (cross a b c) where
    makeDirection x | x == 0 = Straight
                    | x < 0 = LeftTurn
                    | x > 0 = RightTurn

cross :: Point -> Point -> Point -> Double
cross (Point x1 y1) (Point x2 y2) (Point x3 y3) =  (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

compareCross :: Point -> Point -> Point -> Ordering
compareCross pvt a b = if angle == EQ then distance else angle where
    angle = compare (cross pvt a b) 0                 
    distance = compare (dist pvt a) (dist pvt b)

getTurns :: [Point] -> [(Point,Direction)]
getTurns (x:y:z:ps) = (y,turn x y z) : getTurns (y:z:ps)
getTurns _ = []

-- grahamScan :: [Point] -> [Direction]
grahamScan :: [Point] -> [Point]
grahamScan points = map fst (filter (\(x,d) -> d /= RightTurn) (getTurns sortedPoints)) where
    p = nub points
    pvt = lowestY p
    sortedPoints = pvt : (sortBy (compareCross pvt) (delete pvt p) ++ [pvt,pvt]

-- We have two ways of sorting, by minimum y co-ordinate (or X if there is a draw)
compareYPoint :: Point -> Point -> Ordering
compareYPoint (Point x1 y1) (Point x2 y2)
    | y1 == y2 = compare x1 x2
    | y1 <= y2  =  LT
    | otherwise =  GT

-- Or by cotangent to point with the min co-ordinate
compareAngle :: Point -> Point -> Point -> Ordering
compareAngle (Point px py) p1 p2 = compare (angle p2) (angle p1) where
    angle (Point x1 y1) = y1-py / x1-px


lowestY :: [Point] -> Point
lowestY = minimumBy compareYPoint

pointsFromTupleList :: [(Double,Double)] -> [Point]
pointsFromTupleList = map (uncurry Point)

examplePoints :: [Point]
examplePoints = pointsFromTupleList [(0,0),(2,0),(2,2),(0,2),(1,1)]
