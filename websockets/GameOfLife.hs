import Char
import Web
import System.IO

import Data.Array

data Cell = Off 
          | On
          | Dying
          deriving (Eq,Show)

cellToChar :: Cell -> Char
cellToChar Off = '0'
cellToChar On = '1'
cellToChar Dying = '2' 

charToCell :: Char -> Cell
charToCell '0' = Off
charToCell '1' = On
charToCell '2' = Dying
charToCell _ = error "Undefined character received"

type GameGrid = Array (Int,Int) Cell

type Neighbours = [Cell]

data Game = Game GameGrid Int deriving Show

createGame :: Int -> [Cell] -> Game
createGame x c = Game (listArray ((0,0),(x-1,x-1)) c) x

gridToString :: Game -> String
gridToString (Game g _) = map cellToChar (elems g)

neighbours :: Game -> (Int,Int) -> Neighbours
neighbours (Game c s) (x,y) = [c ! ((x+dx) `mod` s, (y+dy) `mod` s)
                                   | dx <- [-1,0,1], dy <- [-1,0,1], dx /= dy]

rules :: Cell -> Neighbours -> Cell
rules On _ = Dying
rules Off cells | length (filter (/= Off) cells) == 2 = On
                | otherwise = Off
rules Dying _ = Off
    
step :: Game -> [Cell]
step g@(Game c s) = [ rules (c ! p) (neighbours g p) | p <- coords] where
    coords = [(x,y) | x <- [0..(s-1)], y <- [0..(s-1)]]

processMessage :: String -> String
processMessage s = map cellToChar newGrid where
    [cellSizeStr,original] = lines s
    cells = map charToCell original
    cellSize = read cellSizeStr :: Int
    newGrid = step (createGame cellSize cells)

listenLoop :: Handle -> IO ()
listenLoop h = do
  msg <- readFrame h
  sendFrame h (processMessage msg)
  listenLoop h

main :: IO ()
main = serverListen 9876 listenLoop
