import Maybe
import List

import Data.Word
import Data.Array

import Graphics.Pgm

data Point = Point { x :: Float
                   , y :: Float
                   , z :: Float
                   } deriving (Show)

data Sphere = Sphere { color :: Float
                     , radius :: Float
                     , centre :: Point 
                     } deriving (Show)

data ObjectHit = ObjectHit { object :: Sphere
                           , location :: Point 
                           } deriving (Show)

data Brightness = Brightness { value :: Float } deriving (Show)

square :: (Num a) => a -> a
square x = x * x

magnitude :: Point -> Float
magnitude p = sqrt ((square (x p)) + (square (y p)) + (square (z p)))

unitVector :: Point -> Point
unitVector p = let d = magnitude p
               in Point ((x p)/d) ((y p)/d) ((z p)/d)

pointSubtract :: Point -> Point -> Point
pointSubtract p1 p2 = Point (x p1-x p2) (y p1-y p2) (z p1-z p2)

distance :: Point -> Point -> Float
distance p1 p2 = magnitude (pointSubtract p1 p2)  

sphereNormal :: Sphere -> Point -> Point
sphereNormal s p = unitVector (pointSubtract (centre s) p)                

lambert :: Sphere -> Point -> Point -> Float
lambert s i r = let n = sphereNormal s i
                in max 0 ((x r * x n) + (y r * y n) + (z r * z n))

minroot :: Float -> Float -> Float -> Maybe Float
minroot a b c 
    | a == 0 = Just ((- c) / b)
    | otherwise = let disc = (square b) - (4 * a * c)
                  in if (disc > 0) 
                     then Just (min (((-b) + sqrt disc) / (2 * a)) (((-b) - sqrt disc) / (2 * a)))
                     else Nothing

sphereIntersect :: Sphere -> Point -> Point -> Maybe ObjectHit
sphereIntersect s pt r = let c = centre s
                             n = minroot (square (x r) + square (y r) + square (z r)) 
                                         (2 * ((x r * (x pt - x c)) + (y r * (y pt - y c)) + (z r * (z pt - z c))))
                                         ((square (x pt - x c)) + (square (y pt - y c)) + (square (z pt - z c)) - (square (radius s)))
                         in if (isNothing n)
                            then Nothing
                            else Just (ObjectHit 
                                       s
                                       (Point
                                        ((x pt) + (fromJust n) * (x r))
                                        ((y pt) + (fromJust n) * (y r))
                                        ((z pt) + (fromJust n) * (z r))))

spheresHit :: [Sphere] -> Point -> Point -> [ObjectHit]
spheresHit sw pt r = mapMaybe (\x -> sphereIntersect x pt r) sw
                                
nearestHit :: [Sphere] -> Point -> Point -> Maybe ObjectHit
nearestHit sp pt r = let hitSpheres = spheresHit sp pt r
                     in 
                         case hitSpheres of
                           [] -> Nothing
                           x  -> Just (head (sortBy 
                                             (\h1 h2 -> (compare (distance (location h1) pt) (distance (location h2) pt)))
                                             x))
                                           
sendRay :: [Sphere] -> Point -> Point -> Brightness                                   
sendRay world src ray = let hit = nearestHit world src ray
                  in if (isNothing hit)
                     then (Brightness 0)
                     else let sp = object (fromJust hit) in
                          (Brightness ((color sp) * (lambert sp src ray)))

colorAt :: [Sphere] -> Point -> Float -> Float -> Brightness
colorAt world eye x y = let ray = unitVector (pointSubtract (Point x y 0) eye)
                        in (Brightness (255 * value (sendRay world eye ray)))        

exampleEye :: Point
exampleEye = (Point 150 150 200)

exampleWorld :: [Sphere]
exampleWorld = [Sphere 0.32 250 (Point 150 150 (-600)),
                Sphere 0.64 100 (Point 175 175 (-300))]

image :: [Sphere] -> Point -> Int -> Int -> Array (Int,Int) Int 
image world eye width height = 
    array 
      ((0,0),(width,height)) 
      [((i,j),truncate (255 * (value (colorAt world eye (fromIntegral i) (fromIntegral j))))) | 
        i <- [0..width], j<- [0..height]]

imageWord16 :: Array (Int,Int) Int -> Array (Int,Int) Word16
imageWord16 image = fmap (fromIntegral :: Int -> Word16) image

saveImage :: String -> [Sphere] -> Point -> Int -> Int -> IO ()
saveImage filename world eye width height = arrayToFile filename (imageWord16 (image world eye width height))