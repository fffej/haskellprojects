module TrafficVis where

import Traffic

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless, when, forM_,liftM)
import Data.IORef (IORef, newIORef)

import qualified Data.Map as M

data State = State {
      env :: IORef Environment
}

makeState :: IO State
makeState = liftM State (newIORef createEnvironment)

displayFunc :: State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  environment <- G.get (env s)
  _ <- drawCars (cars environment)
  _ <- drawRoutes (routes environment)
  _ <- drawLocations (locations environment)
  swapBuffers

drawCars :: [Car] -> IO ()
drawCars = mapM_ drawCar 

drawCar :: Car -> IO ()
drawCar car = undefined
    where
      (x,y) = carPosition car

drawRoutes :: Route -> IO ()
drawRoutes route = mapM_ (\((l1,l2),speed) -> drawRoute l1 l2 speed) (M.toList route)

drawRoute :: Location -> Location -> Double -> IO ()
drawRoute = undefined

drawLocations :: [Location] -> IO ()
drawLocations = mapM_ drawLocation

drawLocation :: Location -> IO ()
drawLocation (Location (x,y) _) = undefined

-- remember to postRedisplay Nothing if changed
-- no logic should go here
idleFunc :: State -> IdleCallback
idleFunc s = env s $~ update

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
   unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 1 0 1
      clearColor $= Color4 0 0 0 1

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBAMode ]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Stop the traffic!"
  clearColor $= Color4 0 0 0 1

  state <- makeState

  displayCallback $= displayFunc state
  idleCallback $= Just (idleFunc state)
  reshapeCallback $= Just reshapeFunc

  mainLoop