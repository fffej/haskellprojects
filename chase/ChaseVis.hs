module ChaseVis where

import Chase

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless, when, forM_,liftM,liftM2)
import Data.IORef (IORef, newIORef)

import Data.Map (Map,(!))
import qualified Data.Map as M

data State = State {
      env :: IORef Environment
}

makeState :: IO State
makeState = liftM State (newIORef $ createEnvironment 32)

winHeight :: Int
winHeight = 512

winWidth :: Int
winWidth = 512

tick :: Int
tick = 25

color3f :: Color3 GLfloat -> IO ()
color3f = color

vertex2f :: Vertex2 GLfloat -> IO ()
vertex2f = vertex :: Vertex2 GLfloat -> IO ()

colorVertex :: Color3 GLfloat ->  Vertex2 GLfloat -> IO ()  
colorVertex c v = color3f c >> vertex v

displayFunc :: State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  e <- G.get (env s)
  _ <- drawGrid e 
  flush
  swapBuffers

pickColor :: Agent -> Color3 GLfloat
pickColor (Goal s) = Color3 (realToFrac s) 0 0
pickColor (Pursuer s) = Color3 0 (realToFrac s) 0
pickColor (Path s) = Color3 0 0 (realToFrac s)
pickColor Obstacle = Color3 1 1 1

drawGrid :: Environment -> IO ()
drawGrid (Environment g b) = do
  lineWidth $= realToFrac 0.1
  let sqSize = (fromIntegral winHeight / fromIntegral b)
  let f i = ((fromIntegral i - 0.5 :: GLfloat) * sqSize)
  renderPrimitive Quads $ forM_ [(x,y) | x <- [0..b], y <- [0..b]]
                      (\(i,j) ->
                             do
                               let c = pickColor (top $ g ! (i,j))
                               colorVertex c (Vertex2 (f i) (f j))
                               colorVertex c (Vertex2 (f i+sqSize) (f j))
                               colorVertex c (Vertex2 (f i+sqSize) (f j+sqSize))
                               colorVertex c (Vertex2 (f i) (f j+sqSize)))
  flush
    

timerFunc :: State -> IO ()
timerFunc s = do
  env s $~ update
  postRedisplay Nothing
  addTimerCallback tick (timerFunc s)

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) = 
    unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      loadIdentity
      ortho2D 0 512 0 512
      clearColor $= Color4 0 0 0 1

keyboardMouseHandler :: State -> KeyboardMouseCallback
keyboardMouseHandler  _ _ _ _ _ = return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBAMode ]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Agent Visualization"

  state <- makeState

  displayCallback $= displayFunc state
  reshapeCallback $= Just reshapeFunc
  keyboardMouseCallback $= Just (keyboardMouseHandler state)
  addTimerCallback tick (timerFunc state)

  mainLoop