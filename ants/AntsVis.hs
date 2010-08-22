module AntsVis where

import Ants

import Graphics.UI.GLUT as G
import System.Exit (exitWith,ExitCode(ExitSuccess))
import Control.Monad
import Data.IORef

import Data.Array

import Control.Concurrent.STM

-- state is the world

-- |Timeout in ms for the callback
tick :: Int
tick = 100

displayFunc :: World -> DisplayCallback
displayFunc w = forM_ (assocs $ cells w) (uncurry drawPlace)

timerFunc :: World -> IO ()
timerFunc w = do
  postRedisplay Nothing
  addTimerCallback tick (timerFunc w)
  return ()

drawAnt :: (Int,Int) -> Ant -> IO ()
drawAnt loc ant = undefined

drawPlace :: (Int,Int) -> TCell -> IO ()
drawPlace loc tcell = do
  cell <- atomically $ readTVar tcell
  when (pheromone cell > 0)
           (print "Color in cell blue ish based on the pheromone")
  when (food cell > 0)
           (print "Color in cell red based on the amount of food")
  when (hasAnt cell)
           (print "Return the ant")

keyboardMouseHandler :: World -> KeyboardMouseCallback
keyboardMouseHandler _ _ _ _ _ = return ()

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
    unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 (fromIntegral dim) 0 (fromIntegral dim)
      clearColor $= Color4 0 0 0 1

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Ants in Haskell."
  clearColor $= Color4 0 0 0 1

  world <- atomically mkWorld

  displayCallback $= displayFunc world
  reshapeCallback $= Just reshapeFunc
  keyboardMouseCallback $= Just (keyboardMouseHandler world)
  addTimerCallback tick (timerFunc world)

  mainLoop
