import Char
import Web
import Network
import System.IO

import Data.Array
import Data.List.Split

data Cell = Off 
          | On 
          | Dying
          deriving (Show)

-- For marshalling back and forth to JSON
-- this is embarassingly shite
cellToChar :: Cell -> Char
cellToChar Off = '0'
cellToChar On = '1'
cellToChar Dying = '2' 

charToCell :: Char -> Cell
charToCell '0' = Off
charToCell '1' = On
charToCell '2' = Dying
charToCell x = error "Undefined character received"

type GameGrid = Array (Int,Int) Cell

data Game = Game GameGrid Int deriving Show

createGame :: Int -> [Cell] -> Game
createGame x c = Game (listArray ((0,0),(x - 1,x - 1)) c) x

gridToString :: Game -> String
gridToString (Game g c) = elems $ fmap cellToChar g

-- If a square is On -> Dying
-- If a square is off, it turns on if it has EXACTLY two neighbours
-- If a square Dying -> Off

neighbours :: Game -> (Int,Int) -> [Cell]
neighbours (Game c s) (x,y) = [c ! ((x + dx) `mod` s, (y + dy) `mod` s)
                                   | dx <- [-1,0,1], dy <- [-1,0,1], dx /= dy]
    
step :: Game -> Game
step grid = grid

processMessage :: String -> String
processMessage s = gridToString newGrid where
    [cellSizeStr,original] = lines s
    cells = map charToCell original
    cellSize = read cellSizeStr :: Int
    newGrid = step (createGame cellSize cells)

listenLoop :: Handle -> IO ()
listenLoop h = do
  msg <- readFrame h
  putStrLn ("Received: " ++ msg)
  putStrLn ("Calculated: " ++ (processMessage msg))
  sendFrame h (processMessage msg)
  listenLoop h
  

main = serverListen 9876 listenLoop



