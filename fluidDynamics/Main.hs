module Main where

import MFluid (densStep,velStep,readVal,writeVal,Grid,emptyGrid,zeroGrid)

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless,when,forM_)
import Data.IORef (IORef, newIORef)

color3f :: Color3 GLfloat -> IO ()
color3f = color

vertex2f :: Vertex2 GLfloat -> IO ()
vertex2f = vertex :: Vertex2 GLfloat -> IO ()

-- |Grid resolution
n :: Int
n = 80

-- |Time step
dt :: Double
dt = 0.1

-- |Diffusion rate of the density
diff :: Double
diff = 0.0001

-- |Viscosity of the fluid
visc :: Double
visc = 0.002

-- |Scales the mouse movement that generates a force
force :: Double
force = 5.0

-- |Amount of density that will be deposited
source :: Double
source = 100.0

colorVertex :: (Color3 GLfloat, Vertex2 GLfloat) -> IO ()  
colorVertex (c,v) = do
  color3f c
  vertex v

data State = State {
      dens :: Grid
    , densPrev :: Grid
    , vel :: (Grid,Grid)
    , velPrev :: (Grid,Grid)
    , mousePoint :: IORef (Int,Int)
    , oMousePoint :: IORef (Int,Int)
    , leftDown :: IORef Bool
    , rightDown :: IORef Bool
    , drawVel :: IORef Bool
}

makeState :: IO State
makeState = do
  densGrid <- emptyGrid n
  densPrevGrid <- emptyGrid n
  vG1 <- emptyGrid n
  vG2 <- emptyGrid n
  vP1 <- emptyGrid n
  vP2 <- emptyGrid n
  mP <- newIORef (0,0)
  omP <- newIORef (0,0)
  left <- newIORef False
  right <- newIORef False
  mD <- newIORef False
  return $ State densGrid densPrevGrid (vG1,vG2) (vP1,vP2) mP omP left right mD

clearState :: State -> IO()
clearState s = do
{-  g <- emptyGrid n
  dens s $~ const g
  densPrev s $~ const g
  vel s $~ const (g,g)
  velPrev s $~ const (g,g)-}
  mousePoint s $~ const (0,0)
  oMousePoint s $~ const (0,0)
  leftDown s $~ const False
  rightDown s $~ const False
  drawVel s $~ const False
  return ()

trun :: Double -> Double -> GLfloat
trun h i = realToFrac ((i-0.5) * h) :: GLfloat

drawVelocity :: (Grid,Grid) -> IO ()
drawVelocity (u,v) = do
  color3f (Color3 1 1 1)
  lineWidth $= 1.0
  let h = 1.0 / realToFrac n
  let f = trun h
  renderPrimitive Lines $ forM_ [(x,y) | x<-[1..n], y<-[1..n] ]
                     (\(i,j) ->
                          do
                            uV <- readVal u (i,j)
                            vV <- readVal v (i,j)
                            vertex2f (Vertex2 (f (realToFrac i)) (f (realToFrac j)))
                            vertex2f (Vertex2 (f ((realToFrac i) + uV)) (f ((realToFrac j) + vV))))
  
densColor :: Grid -> (Int,Int) -> IO (GLfloat,GLfloat,GLfloat,GLfloat)
densColor g p@(x,y) = do
  d00 <- readVal g p
  d01 <- readVal g (x,y+1)
  d10 <- readVal g (x+1,y)
  d11 <- readVal g (x+1,y+1)
  return (realToFrac d00,realToFrac d01,realToFrac d10,realToFrac d11) 

mapToColor :: (GLfloat,GLfloat) -> GLfloat -> GLfloat -> GLfloat -> (Color3 GLfloat)
mapToColor (i,j) x y z = Color3 (i*x) (j*y) (i/j * z)
                         

drawDens :: Grid -> IO ()
drawDens g = do
  color3f (Color3 1 0 1)
  lineWidth $= 0.5
  let h = 1.0 / fromIntegral n
  let f i = (fromIntegral i - 0.5 :: GLfloat) * h
  renderPrimitive Quads $ forM_ [(x,y) | x<-[1..n], y<-[1..n]]
                      (\(i,j) ->
                           do
                             (d00,d01,d10,d11) <- densColor g (i,j)
                             let m = (fromIntegral i / fromIntegral n, fromIntegral j / fromIntegral n)
                             colorVertex (mapToColor m d00 d00 d00, Vertex2 (f i) (f j))
                             colorVertex (mapToColor m d10 d10 d10, Vertex2 (f i+h) (f j))
                             colorVertex (mapToColor m d11 d11 d11, Vertex2 (f i+h) (f j+h))
                             colorVertex (mapToColor m d01 d01 d01, Vertex2 (f i) (f j+h)))
  flush
                  
displayFunc :: State -> DisplayCallback
displayFunc s = do
  clear [ColorBuffer]
  let d = dens s
      v = vel s
  dv <- G.get (drawVel s)
  drawDens d
  when (dv) (drawVelocity v)     
  swapBuffers

pos :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
pos n (width,height) (x,y) = (truncate (dx/dw*dn), n - truncate (dy/dh*dn)) where
    dx = fromIntegral x :: Double
    dy = fromIntegral y :: Double
    dn = fromIntegral n :: Double 
    dw = fromIntegral width :: Double
    dh = fromIntegral height :: Double

updateForce :: (Int,Int) -> (Double,Double) -> (Grid,Grid) -> IO ()
updateForce p (dx,dy) (u,v) = do
  writeVal u p (force * dx)
  writeVal v p (force * dy)

updateDens :: (Int,Int) -> Grid -> IO () 
updateDens p g = do
  c <- readVal g p
  writeVal g p (c + source)

updateStateFromUI :: State -> IO()
updateStateFromUI s = do
  (_, Size width height) <- G.get viewport  
  (mx,my) <- G.get (mousePoint s)
  (omx,omy) <- G.get (oMousePoint s)
  let (x,y) = pos n (fromIntegral width :: Int, fromIntegral height :: Int) (mx,my)
  left <- G.get (leftDown s)
  right <- G.get (rightDown s)
  let velP = velPrev s
      denP = densPrev s
  when (left)
       (updateForce (x,y)  (realToFrac (mx - omx), realToFrac (omy - my)) velP)
  when (right)
       (updateDens (x,y) denP)
  oMousePoint s $~ (const (mx,my))
  return ()

-- Update the display
idleFunc :: State -> IdleCallback
idleFunc s = do

  -- Reset the previous velocities
  let (u0,v0) = velPrev s
      densP = densPrev s
      density = dens s
      (u,v) = vel s
  zeroGrid u0
  zeroGrid v0
  zeroGrid densP

  left <- G.get (leftDown s)
  right <- G.get (rightDown s)

  -- If necessary, update the prev values
  when (left || right) 
       (updateStateFromUI s)

  velStep u v u0 v0 visc dt
  densStep density densP u v diff dt

  postRedisplay Nothing -- TODO should only do this if changed
  return ()

reshapeFunc :: ReshapeCallback
reshapeFunc size@(Size _ height) =
   unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      ortho2D 0 1 0 1
      clearColor $= Color4 0 0 0 1

setMouseData :: State -> Key -> (Int,Int) -> IO ()
setMouseData s k (x,y)= do
  mousePoint s $~ const (x,y)
  oMousePoint s $~ const (x,y)
  setButton s k

setButton :: State -> Key -> IO ()
setButton s (MouseButton LeftButton) = leftDown s $~ not
setButton s (MouseButton RightButton) = rightDown s $~ not
setButton _ _ = return ()

keyMouseFunc :: State -> KeyboardMouseCallback
keyMouseFunc _ (Char 'q') _ _ _ = exitWith ExitSuccess
keyMouseFunc s (Char 'c') _ _ _ = clearState s
keyMouseFunc s (Char 'v') _ _ _ = drawVel s $~ not
keyMouseFunc s m _ _ (Position x y) = setMouseData s m (fromIntegral x :: Int,fromIntegral y :: Int)

motionFunc :: State -> MotionCallback
motionFunc s (Position x y) = do
  mousePoint s $~ const (fromIntegral x :: Int,fromIntegral y :: Int)
  return ()
 
-- This just starts up the event loop
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBAMode ]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= Position 0 0
  _ <- createWindow "Barely Functional Fluid Dynamics"
  clearColor $= Color4 0 0 0 1

  state <- makeState

  -- Register the callback functions
  displayCallback $= displayFunc state
  idleCallback $= Just (idleFunc state)
  reshapeCallback $= Just reshapeFunc
  keyboardMouseCallback $= Just (keyMouseFunc state)
  motionCallback $= Just (motionFunc state)

  mainLoop
