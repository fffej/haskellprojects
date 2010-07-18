module Scrabble where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Tile = Tile Char Int

data Score = Normal
           | DoubleLetter
           | DoubleWord
           | TripleLetter
           | TripleWord

data Square = Square Score (Maybe Tile)

type Board = Map (Int,Int) Square

-- All the valid tiles
tileSet = [('A', Tile 'A' 1)
          ,('B', Tile 'B' 3)
          ,('C', Tile 'C' 3)
          ,('D', Tile 'D' 2)
          ,('E', Tile 'E' 1)
          ,('F', Tile 'F' 4)
          ,('G', Tile 'G' 2)
          ,('H', Tile 'H' 4)
          ,('I', Tile 'I' 1)
          ,('J', Tile 'J' 8)
          ,('K', Tile 'K' 5)
          ,('L', Tile 'L' 1)
          ,('M', Tile 'M' 3)
          ,('N', Tile 'N' 1)
          ,('O', Tile 'O' 1)
          ,('P', Tile 'P' 3)
          ,('Q', Tile 'Q' 1)
          ,('R', Tile 'R' 1)
          ,('S', Tile 'S' 1)
          ,('T', Tile 'T' 1)
          ,('U', Tile 'U' 1)
          ,('V', Tile 'V' 4)
          ,('W', Tile 'W' 4)
          ,('X', Tile 'X' 8)
          ,('Y', Tile 'Y' 4)
          ,('Z', Tile 'Z' 10)
          ,(' ', Tile ' ' 0)]