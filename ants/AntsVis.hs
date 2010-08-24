module AntsVis where

import Ants

import Graphics.UI.GLUT as G
import System.Exit (exitWith,ExitCode(ExitSuccess))
import Control.Monad
import Data.IORef
import Data.Maybe (fromJust)

import Data.Array

import Control.Concurrent.STM

color4f :: Color4 GLfloat -> IO ()
color4f = color

vertex2f :: Vertex2 GLfloat -> IO ()
vertex2f = vertex :: Vertex2 GLfloat -> IO ()

colorVertex :: Color4 GLfloat -> Vertex2 GLfloat -> IO ()  
colorVertex c v = do
  color4f c
  vertex v

-- state is the world
-- |Timeout in ms for the callback
tick :: Int
tick = 100

gridSize :: GLfloat
gridSize = 5

pherScale :: GLfloat
pherScale = 20.0

foodScale :: GLfloat
foodScale = 30.0

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
displayFunc w = do
  clear [ColorBuffer]
  forM_ (assocs $ cells w) (uncurry drawPlace) 
  let h = fromIntegral homeOff * gridSize
      g = gridSize + gridSize * fromIntegral nantsSqrt
  renderPrimitive Quads $ do
                  colorVertex (Color4 0 0 1 0) (Vertex2 h h)
                  colorVertex (Color4 0 0 1 0) (Vertex2 (h+g) h)
                  colorVertex (Color4 0 0 1 0) (Vertex2 (h+g) (h+g))
                  colorVertex (Color4 0 0 1 0) (Vertex2 h (h+g))
  swapBuffers

timerFunc :: World -> IO ()
timerFunc w = do
  postRedisplay Nothing
  addTimerCallback tick (timerFunc w)
  return ()

drawAnt :: (Int,Int) -> Ant -> IO ()
drawAnt (x,y) ant = do
  let gray  = Color4 0.4 0.4 0.4 1 :: Color4 GLfloat
      red   = Color4 1 0 0 1 :: Color4 GLfloat
      (hx,hy,tx,ty) = antInfo (direction ant)
      c = if hasFood ant
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
       (fillCell loc (Color4 0 1 0 (realToFrac (pheromone cell) / pherScale)))
  when (food cell > 0)
       (fillCell loc (Color4 1 0 0 (fromIntegral (food cell) / foodScale)))
  when (hasAnt cell)
       (drawAnt loc (fromJust $ ant cell))

keyboardMouseHandler :: World -> KeyboardMouseCallback
keyboardMouseHandler _ _ _ _ _ = return ()

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
    unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 400 0 400 -- (fromIntegral dim) 0 (fromIntegral dim)
      clearColor $= Color4 0 0 0 1

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Ants in Haskell."
  clearColor $= Color4 0 0 0 1

  world <- mkWorld
  _ <- populateWorld world

  displayCallback $= displayFunc world
  reshapeCallback $= Just reshapeFunc
  keyboardMouseCallback $= Just (keyboardMouseHandler world)
  addTimerCallback tick (timerFunc world)

  mainLoop
