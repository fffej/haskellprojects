module Main where

import Orbit as O

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless,when,forM_)
import Data.IORef (IORef, newIORef)
import Data.List.Split (chunk)

import System.Random

delta :: Int
delta = 25

objectCount :: Int
objectCount = 60

data State = State {
      world :: IORef [O.Object]
} 

center :: Vec O.Position 
center = Vec 0 0

type ObjectSeed = (Double,Double,Double,Double) 

makeState :: IO State
makeState = do
  gen <- newStdGen
  let ns = map (\(a:b:c:d:[]) -> (a,b,c,d)) $ chunk 4 (randoms gen :: [Double])
  p <- newIORef (createWorld (take objectCount ns))
  return $ State p

-- Create the world using the given source of randomness
createWorld :: [ObjectSeed] -> [O.Object]
createWorld rnds = sun : map (\(s,n) -> randomObject s sun n) (zip rnds [1..])
    where
      sun = O.Object center 30 (Vec 0 0) (Vec 0 0) 


randomPosition :: Double -> Double -> Vec O.Position -> Vec O.Position
randomPosition x y sunPos = add sunPos (Vec (r * cos theta) (r * sin theta))
    where
      r = x * 150 + 80
      theta = y * 2 * pi

randomVelocity :: Double -> Vec O.Position -> O.Object -> Vec O.Velocity
randomVelocity r p sun = convert (O.scale direction (r*0.3 + 0.3)) O.Velocity where
    direction = rotate90 (unit $ sub p (O.position sun))

randomObject :: (Double,Double,Double,Double) -> O.Object -> Int -> O.Object
randomObject (mass,vel,a,b) sun n = o
    where
      p = randomPosition a b (O.position sun)      
      o = O.Object p (mass * 0.2) (randomVelocity vel p sun) zero
               

-- TODO color
drawObject :: O.Object -> IO ()
drawObject o = preservingMatrix $ do
                 translate (Vector3 (realToFrac x) (realToFrac y) 0.0 :: Vector3 GLfloat)
                 renderObject Solid $ Sphere' radius 100 100
    where
      (Vec x y) = O.position o
      radius = realToFrac $ sizeByMass (mass o)

colorByMass :: Double -> Color4 Double
colorByMass m = Color4 r g b 1 where
    b = min 255 (20.0 * m) / 255.0
    r = min 100 (255.0 - b) / 255.0
    g = 128.0
    
sizeByMass :: Double -> Double
sizeByMass = (+) 3.0 

displayFunc :: State -> DisplayCallback
displayFunc state = do
  clear [ColorBuffer,DepthBuffer]
  materialAmbient Front $= Color4 1 0 0 1
  materialDiffuse Front $= Color4 0 1 0 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 1000
  s <- G.get (world state)
  mapM_ drawObject s
  swapBuffers

initGraphics :: IO ()
initGraphics = do
  depthFunc $= Just Less
  clearDepth $= 100
  matrixMode $= Modelview 0
  loadIdentity
  lighting $= Enabled
  light (Light 0) $= Enabled
  G.position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= Color4 1 1 1 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  matrixMode $= Projection
  loadIdentity
  ortho (-500) 500 (-500) 500 200 (-200)

timerCallback :: State -> TimerCallback
timerCallback state = do
  world state $~ updateAll
  postRedisplay Nothing
  addTimerCallback delta (timerCallback state)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= G.Position 0 0
  _ <- createWindow "Orbit in Haskell"
  _ <- initGraphics

  state <- makeState

  displayCallback $= displayFunc state
  addTimerCallback delta (timerCallback state)

  mainLoop