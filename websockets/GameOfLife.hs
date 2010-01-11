import Char
import Web
import System.IO

import Data.Array

data Cell = Off 
          | On 
          | Dying
          deriving (Eq,Show)

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
charToCell _ = error "Undefined character received"

type GameGrid = Array (Int,Int) Cell

data Game = Game GameGrid Int deriving Show

createGame :: Int -> [Cell] -> Game
createGame x c = Game (listArray ((0,0),(x - 1,x - 1)) c) x

gridToString :: Game -> String
gridToString (Game g _) = elems $ fmap cellToChar g

neighbours :: Game -> (Int,Int) -> [Cell]
neighbours (Game c s) (x,y) = [c ! ((x + dx) `mod` s, (y + dy) `mod` s)
                                   | dx <- [-1,0,1], dy <- [-1,0,1], dx /= dy]

rules :: Cell -> [Cell] -> Cell
rules On _ = Dying
rules Off cells | length (filter (/= Off) cells) == 2 = On
                | otherwise = Off
rules Dying _ = Off
    
step :: Game -> Game
step g@(Game grid size) = Game (grid // updates) size where
    updates = stepCell g

stepCell :: Game -> [((Int,Int),Cell)]
stepCell g@(Game c _) = map (transform g) (assocs c) where
    transform gm ((x,y),s) = ((x,y),rules s (neighbours gm (x,y)))

processMessage :: String -> String
processMessage s = gridToString newGrid where
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



