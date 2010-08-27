module AntsVis where

import Ants

import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Random

import Graphics.UI.GLUT as G
import Data.Maybe (fromJust)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Debug.Trace

color4f :: Color4 GLfloat -> IO ()
color4f = color

vertex2f :: Vertex2 GLfloat -> IO ()
vertex2f = vertex :: Vertex2 GLfloat -> IO ()

colorVertex :: Color4 GLfloat -> Vertex2 GLfloat -> IO ()  
colorVertex c v = do
  color4f c
  vertex v

pos :: Vector (Int,Int)
pos = V.fromList [(y,x) | x <- [0..dim-1], y <- [0..dim-1]]

antBehave :: World -> (Int,Int) -> IO ()
antBehave world p = do
  gen <- newStdGen 
  newPos <- atomically $ behave gen world p                      
  _ <- threadDelay (antTick  * 1000)
  antBehave world newPos

-- |Timeout in ms for the callback
tick :: Int
tick = 50

-- |Timeout for the ants 
antTick :: Int
antTick = 100

gridSize :: GLfloat
gridSize = 5

pherScale :: GLfloat
pherScale = 40.0

foodScale :: GLfloat
foodScale = 100.0

antInfo :: Direction -> (GLfloat,GLfloat,GLfloat,GLfloat)
antInfo N  = (2,0,2,4)
antInfo NE = (4,0,0,4)
antInfo E  = (4,2,0,2)
antInfo SE = (4,4,0,0)
antInfo Ants.S = (2,4,2,0) -- what is the S from Graphics.UI.Glut?
antInfo SW = (0,4,4,0)
antInfo W  = (0,2,4,2)
antInfo NW = (0,0,4,4)

displayFunc :: World -> DisplayCallback
displayFunc world = do
  clear [ColorBuffer]

  let h = fromIntegral homeOff * gridSize
      g = gridSize + gridSize * fromIntegral nantsSqrt
  renderPrimitive Quads $ do
                  colorVertex (Color4 0 0 1 0.1) (Vertex2 h h)
                  colorVertex (Color4 0 0 1 0.1) (Vertex2 (h+g) h)
                  colorVertex (Color4 0 0 1 0.1) (Vertex2 (h+g) (h+g))
                  colorVertex (Color4 0 0 1 0.1) (Vertex2 h (h+g))

  -- Then draw the relevant cells
  V.forM_ (V.zip pos (cells world)) (uncurry drawPlace) 
  swapBuffers

timerFunc :: World -> IO ()
timerFunc w = do
  postRedisplay Nothing
  atomically $ evaporate w
  addTimerCallback tick (timerFunc w)
  return ()

drawAnt :: (Int,Int) -> Ant -> IO ()
drawAnt (x,y) a = do
  let gray  = Color4 0.4 0.4 0.4 1 :: Color4 GLfloat
      red   = Color4 1 0 0 1 :: Color4 GLfloat
      (hx,hy,tx,ty) = antInfo (direction a)
      c = if hasFood a
          then red
          else gray
      x' = fromIntegral x * gridSize
      y' = fromIntegral y * gridSize
     
  renderPrimitive Lines $ do
                    colorVertex c (Vertex2 (hx + x') (hy + y'))
                    colorVertex c (Vertex2 (tx + x') (ty + y'))
  return ()

fillCell :: (Int,Int) -> Color4 GLfloat -> IO ()
fillCell (i,j) c = do
  let x = fromIntegral i *  gridSize
      y = fromIntegral j *  gridSize     
  renderPrimitive Quads $ do
                     colorVertex c (Vertex2 x y)
                     colorVertex c (Vertex2 (x + gridSize) y)
                     colorVertex c (Vertex2 (x + gridSize) (y + gridSize))
                     colorVertex c (Vertex2 x (y + gridSize))

drawPlace :: (Int,Int) -> TCell -> IO ()
drawPlace loc tcell = do
  cell <- atomically $ readTVar tcell
  when (pheromone cell > 0)
       (fillCell loc (Color4 0 (min 1 (realToFrac (pheromone cell) / pherScale)) 0 0))
  when (food cell > 0)
       (fillCell loc (Color4 (min 1 (fromIntegral (food cell) / foodScale)) 0 0 0))
  when (hasAnt cell)
       (drawAnt loc (fromJust $ ant cell))

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
    unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 400 0 400 

keyboardMouseHandler :: KeyboardMouseCallback
keyboardMouseHandler (Char 'q') Down _ _ = exitWith ExitSuccess
keyboardMouseHandler _ _ _ _ = return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,WithAlphaComponent,RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Ants in Haskell."
  clearColor $= Color4 0 0 0 0

  (w,ants) <- mkWorld

  forM_ ants (\x -> forkIO $ antBehave w x >> return ())

  displayCallback $= displayFunc w
  reshapeCallback $= Just reshapeFunc
  addTimerCallback tick (timerFunc w)
  keyboardMouseCallback $= Just keyboardMouseHandler

  mainLoop
