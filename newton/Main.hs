module Main where

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless,when,forM_)
import Data.IORef (IORef, newIORef)

delta :: Int
delta = 50

data State = State {
      ballPosition :: IORef (GLfloat,GLfloat)
}

makeState :: IO State
makeState = do
  position <- newIORef (0.0,0.0)
  return $ State position

sphereAt :: GLfloat -> GLfloat -> IO ()
sphereAt x y = preservingMatrix $ do
                 translate $ (Vector3 x y 0.0 :: Vector3 GLfloat)
                 renderObject Solid $ Sphere' (fromIntegral 10) 100 100
                 

displayFunc :: State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer,DepthBuffer]
  materialAmbient Front $= Color4 1 0 0 1
  materialDiffuse Front $= Color4 0 1 0 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 1000
  (x,y) <- G.get (ballPosition s)
  sphereAt x y
  swapBuffers

initGraphics :: IO ()
initGraphics = do
  depthFunc $= Just Less
  clearDepth $= 100
  matrixMode $= Modelview 0
  loadIdentity
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= Color4 1 1 1 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  matrixMode $= Projection
  loadIdentity
  ortho (-100) 100 (-100) (100) 100 (-100)

timerCallback :: State -> TimerCallback
timerCallback state = do
  (x,y) <- G.get (ballPosition state)
  ballPosition state $~ const (x + 0.1 :: GLfloat,y+0.1 :: GLfloat)
  postRedisplay Nothing
  addTimerCallback delta (timerCallback state)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Newtons Cradle"
  _ <- initGraphics

  state <- makeState

  displayCallback $= displayFunc state
  addTimerCallback delta (timerCallback state)

  mainLoop