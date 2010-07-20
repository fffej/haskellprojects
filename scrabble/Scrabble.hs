module Scrabble where

import Dictionary

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List.Split

-- There's 100 tiles in a standard distribution
data Tile = Tile Char Int deriving (Eq,Show)

data Score = Normal
           | DoubleLetter
           | DoubleWord
           | TripleLetter
           | TripleWord
             deriving (Show)

data Square = Square Score (Maybe Tile)
            deriving (Show)

type Board = Map (Int,Int) Square

data Player = Player1 Int
            | Player2 Int

data Game = Game {
      board :: Board
    , turn :: Player
    , remainingTiles :: [Tile]
    , player1 :: Player
    , player2 :: Player
}

tileToChar :: Tile -> Char
tileToChar (Tile x _) = x

squareToChar :: Square -> Char
squareToChar (Square _ x) | Nothing == x = '/'
                          | otherwise = tileToChar (fromJust x)

squareToScoreChar :: Square -> Char
squareToScoreChar (Square x _) = scoreToChar x

scoreToChar :: Score -> Char
scoreToChar Normal = '-'
scoreToChar DoubleLetter = 'd'
scoreToChar TripleLetter = 't'
scoreToChar DoubleWord = 'D'
scoreToChar TripleWord = 'T'

instance Show Game where
    show = renderWithTiles

renderGame :: Game -> (Square -> Char) -> String
renderGame (Game b t tiles p1 p2) f = concat $ map (++ "\n") $ splitEvery 15 sqs
    where
      sqs = map f [ b Map.! (x,y) | x <- [0..14], y <- [0..14]]

renderWithTiles :: Game -> String
renderWithTiles g = renderGame g squareToChar

renderWithScore :: Game -> String
renderWithScore g = renderGame g squareToScoreChar

-- All the valid tiles
tileSet = [('A', Tile 'A' 1),('B', Tile 'B' 3),('C', Tile 'C' 3)
          ,('D', Tile 'D' 2),('E', Tile 'E' 1),('F', Tile 'F' 4)
          ,('G', Tile 'G' 2),('H', Tile 'H' 4),('I', Tile 'I' 1)
          ,('J', Tile 'J' 8),('K', Tile 'K' 5),('L', Tile 'L' 1)
          ,('M', Tile 'M' 3),('N', Tile 'N' 1),('O', Tile 'O' 1)
          ,('P', Tile 'P' 3),('Q', Tile 'Q' 1),('R', Tile 'R' 1)
          ,('S', Tile 'S' 1),('T', Tile 'T' 1),('U', Tile 'U' 1)
          ,('V', Tile 'V' 4),('W', Tile 'W' 4),('X', Tile 'X' 8)
          ,('Y', Tile 'Y' 4),('Z', Tile 'Z' 10),(' ', Tile ' ' 0)]

-- Distribution of tiles
tileDistribution = [(9,'A'),(2,'B'),(2,'C'),(4,'D'),(12,'E')
                   ,(2,'F'),(3,'G'),(2,'H'),(9,'I'),(1,'J')
                   ,(1,'K'),(4,'L'),(2,'M'),(6,'N'),(8,'O')
                   ,(2,'P'),(1,'Q'),(6,'R'),(4,'S'),(6,'T')
                   ,(4,'U'),(2,'V'),(2,'W'),(1,'X'),(2,'Y')
                   ,(1,'Z'),(2,' ')]

-- Location of triple word scores
tripleW :: [(Int,Int)]
tripleW = [(0,0),(7,0),(14,14),(0,7),
           (14,0),(7,14),(0,14),(14,7)]

-- Location of double letter scores
doubleL :: [(Int,Int)]
doubleL = [(3,0),(11,0),(3,14),(11,14) -- sides
          ,(0,3),(0,11),(14,3),(14,11)
          ,(6,2),(8,2),(7,3)
          ,(6,12),(8,12),(7,11)
          ,(2,6),(2,8),(3,7)
          ,(12,6),(12,8),(11,7)
          ,(6,6),(8,8),(8,6),(6,8)
          ,(7,7)]

-- Location of triple letter scores
tripleL :: [(Int,Int)]
tripleL = [(5,1),(9,1)
          ,(1,5),(1,9)
          ,(13,5),(13,9)
          ,(5,13),(9,13)
          ,(5,5),(9,5)
          ,(5,9),(9,9)]

-- Location of double word scores
doubleW :: [(Int,Int)]
doubleW = [(1,1),(2,2),(3,3),(4,4)
          ,(13,13),(12,12),(11,11),(10,10)
          ,(13,1),(12,2),(11,3),(10,4)
          ,(1,13),(2,12),(3,11),(4,10)]
               
initialGame :: Game
initialGame = Game initialBoard turn tiles player1 player2
    where
      player1 = Player1 0
      player2 = Player2 0
      turn = player1

initialBoard :: Board
initialBoard = Map.fromList [((x,y),lookupSq (x,y)) | x <- [0..14], y <- [0..14]]

lookupSq :: (Int,Int) -> Square
lookupSq (x,y) | (x,y) `elem` doubleW = Square DoubleWord Nothing
               | (x,y) `elem` tripleW = Square TripleWord Nothing
               | (x,y) `elem` doubleL = Square DoubleLetter Nothing
               | (x,y) `elem` tripleL = Square TripleLetter Nothing
               | otherwise = Square Normal Nothing

tiles :: [Tile]
tiles = concatMap (\(n,t) -> replicate n (fromJust $ lookup t tileSet)) tileDistribution

{-

T--d---T---d--T
-D---t-------D-
--D----d-d--D--
d--D----d--D--d
----D-----D----
-t---t---tt--t-
------d-d------
T-d---------d-T
---d--d-d--d---
--d--t---t--d--
-t--D-----D--t-
d--D----d--D--d
--D----d-d--D--
-D---t----t--D-
T--d---T---d--T


-}