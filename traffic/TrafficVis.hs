module TrafficVis where

-- To compile
-- ghc -lglut --make -main-is TrafficVis -fforce-recomp TrafficVis.hs

import Traffic

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless, when, forM_,liftM)
import Data.IORef (IORef, newIORef)

import qualified Data.Map as M

data State = State {
      env :: IORef Environment
}

-- |Timeout in ms for callback
tick :: Int
tick = 25

makeState :: IO State
makeState = liftM State (newIORef createEnvironment)

displayFunc :: State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  environment <- G.get (env s) 
  _ <- drawCars (cars environment)
  _ <- drawRoutes (routes environment)
  _ <- drawLocations (locations environment)
  _ <- drawInfo environment
  flush
  swapBuffers

color3f :: Color3 GLfloat -> IO ()
color3f = color

vertex2f :: Vertex2 GLfloat -> IO ()
vertex2f = vertex :: Vertex2 GLfloat -> IO ()

vertex2d :: Double -> Double -> Vertex2 GLfloat
vertex2d x y = Vertex2 (realToFrac x) (realToFrac y)

drawCars :: [Car] -> IO ()
drawCars = mapM_ drawCar 

drawCar :: Car -> IO ()
drawCar car = do
  let (x,y) = carPosition car
  color3f (Color3 1 0 0)
  pointSize $= realToFrac 20
  renderPrimitive Triangles $ do
                vertex2f (vertex2d x y)
                vertex2f (vertex2d (x - 5) y)
                vertex2f (vertex2d x (y + 5))

drawRoutes :: Route -> IO ()
drawRoutes route = mapM_ (\((l1,l2),speed) -> drawRoute l1 l2 speed) (M.toList route)

drawRoute :: Location -> Location -> Double -> IO ()
drawRoute (Location (x1,y1) _) (Location (x2,y2) _) m = do
  lineWidth $= realToFrac 0.5
  color3f (Color3 0 1 0)
  renderPrimitive Lines $ do
    vertex2f (vertex2d x1 y1) 
    vertex2f (vertex2d x2 y2)

drawLocations :: [Location] -> IO ()
drawLocations = mapM_ drawLocation

drawLocation :: Location -> IO ()
drawLocation (Location (x,y) _) = do
  color3f (Color3 0 0 1)
  pointSize $= realToFrac 3
  renderPrimitive Points (vertex2f (vertex2d x y))

drawInfo :: Environment -> IO ()
drawInfo e = do
  rasterPos (vertex2d 5 240)
  renderString Fixed8By13 (stats e)
                  
-- remember to postRedisplay Nothing if changed
-- no logic should go here
timerFunc :: State -> IO ()
timerFunc s = do
  env s $~ update
  postRedisplay Nothing
  addTimerCallback tick (timerFunc s)

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
   unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 256 0 256
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
  reshapeCallback $= Just reshapeFunc

  addTimerCallback tick (timerFunc state)

  mainLoop