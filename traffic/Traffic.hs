module Traffic where

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import System.Random

import Test.QuickCheck
import Debug.Trace

type Position = (Double,Double)
type Speed = Double
type Route = Map (Location,Location) Speed

data Location = Location {
      position :: Position
    , name :: String
} deriving (Eq,Ord,Show)
     
data Car = Car {
      distanceToDestination :: Double
    , speed :: Speed
    , route :: (Location,Location)
} deriving (Eq,Show)

data Environment = Environment {
      locations :: [Location]
    , routes :: Route
    , cars :: [Car]
    , noise :: [Double] -- infinite list of randomness
} deriving (Show)

{- Some sample data -}
lA = Location (10,50) "A"
lB = Location (110,50) "B"
lC = Location (170,150) "C"
lD = Location (120,150) "D"
lE = Location (70,75) "E"

createLocations :: [Location]
createLocations = map (\z -> Location (x z,y z) "X")  [0,(pi/15) .. (2*pi)]
    where
      x theta = 100 * cos theta + 128
      y theta = 100 * sin theta + 128

makeRoutes :: [Location] -> Route
makeRoutes locations = M.fromList (zip (zip locations (cycle $ tail locations)) (repeat 5))

makeCars :: Route -> [Car]
makeCars r = map (\((s,f),_) -> Car 1.0 1.0 (s,f)) (M.toList r)

createRoutes :: [((Location,Location), Speed)] -> Route
createRoutes r = M.fromList $ concatMap (\((x,y),s) -> [((x,y),s), ((y,x),s)]) r

createEnvironment = Environment {
                      locations = createLocations
                    , routes = makeRoutes createLocations
                    , cars = makeCars (makeRoutes createLocations)
                    , noise = randoms (mkStdGen 100)
                    }

{- Actual Logic of simulation -}
stats :: Environment -> String
stats e = "Average speed: " ++ show avgSpeed
    where
      c = cars e
      avgSpeed = sum (map speed c) / realToFrac (length c)

update :: Environment -> Environment
update env = env' { cars = updateCars env (cars env) }
    where
      env' = env { noise = drop (length (cars env)) (noise env) }

carsOnRoute :: Car -> [Car] -> [Car]
carsOnRoute car = filter (\c -> route c == route car && c /= car) 
               
updateCars :: Environment -> [Car] -> [Car]
updateCars env cars = map (\(c,n) -> updateCar env n c) (zip cars (noise env))

updateCar :: Environment -> Double -> Car -> Car
updateCar env d car = updateCarSpeed env d (updateCarPosition env d car)

-- |Cars follow simple logic
updateCarSpeed :: Environment -> Double -> Car -> Car
updateCarSpeed env d car | null nearestCars = car 
                         | distanceBetween < 3 = car { speed = min maxSpeed (speed car * (1 + d*0.025)) }
                         | distanceBetween > 3 = car { speed = max 0 (speed car * (1 - d*0.025)) }
                         | otherwise = car
    where
      maxSpeed = fromJust $ M.lookup (route car) (routes env)
      nearestCars = sortBy 
                    (comparing distanceToDestination)
                    (carsOnRoute car (cars env))
      distanceBetween  = distanceToDestination (head nearestCars) - distanceToDestination car

updateCarPosition :: Environment -> Double -> Car -> Car
updateCarPosition env choice car | distanceToGo <= 0 = updateLocation env choice car
                                 | otherwise = car { distanceToDestination = distanceToGo }
    where
      distanceToGo = distanceToDestination car - speed car

updateLocation :: Environment -> Double -> Car -> Car
updateLocation env choice car = car { 
                                  distanceToDestination = distanceToGo
                                , route = (finish,newDestination)
                                }
    where
      (start,finish) = route car
      newDestination = chooseNewDestination env choice finish
      distanceToGo = distanceBetween (position finish) (position newDestination)

-- |TODO non-determinism and unsafe code assuming a root backwards
chooseNewDestination :: Environment -> Double -> Location -> Location
chooseNewDestination env choice s = snd $ fst (choices !! truncate (choice * realToFrac (length choices)))
    where
      choices = filter (\((x,_),_) -> x == s) (M.toList (routes env))


carPosition :: Car -> Position
carPosition (Car d _ (start,finish)) = (x1+p*(x2-x1), y1+p*(y2-y1))
    where
      s@(x1,y1) = position start
      e@(x2,y2) = position finish
      p = 1 - (d / distanceBetween s e)


changeSpeedLimit :: (Speed -> Speed) -> Environment -> Environment
changeSpeedLimit d e = e { routes = updatedRoutes }
    where
      updatedRoutes = M.map d (routes e)

addCar :: Environment -> Environment
addCar e = e { cars = cars'  }
    where
      cars' = Car 1.0 1.0 (s,f) : (cars e)
      ((s,f),_) = head (M.toList (routes e))

removeCar :: Environment -> Environment
removeCar e = e { cars = cars' }
    where
      cars' = drop 1 (cars e)

{-  Boring helper code that plays no part in the *real* work -}
distanceBetween :: Position -> Position -> Double
distanceBetween (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

{- Testing code. -}
getCarLocation :: Double -> Position -> Position -> Position
getCarLocation d s e = carPosition (Car d 0 (Location s "Start",Location e "End"))

-- |The distance we are at is calculated correctly
prop_distanceCorrect :: NonNegative Double -> Position -> Position -> Bool
prop_distanceCorrect (NonNegative d) s e | s == e = True -- prefer different positions!
                                         | abs d > dis = True 
                                         | otherwise = abs (db -  d) < 0.0001
    where 
      dis = distanceBetween s e
      pos = getCarLocation d s e
      db = distanceBetween pos e
