module AntsVis where

import Ants

import Graphics.UI.GLUT as G
import System.Exit (exitWith,ExitCode(ExitSuccess))
import Control.Monad
import Data.IORef

import Control.Concurrent.STM

-- state is the world

-- |Timeout in ms for the callback
tick :: Int
tick = 100

displayFunc :: World -> DisplayCallback
displayFunc w = undefined

timerFunc :: World -> IO ()
timerFunc w = undefined

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
