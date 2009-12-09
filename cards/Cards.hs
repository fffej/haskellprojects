module Cards where

data Suit = Clubs 
          | Diamonds 
          | Hearts  
          | Spades
            deriving (Eq,Show,Enum,Bounded)

data Value = Ace
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
             deriving (Eq,Show,Enum,Bounded)

data Color = Red
           | Black
             deriving (Eq, Show)
           
color :: Card -> Color
color (Card _ s) | s == Clubs || s == Spades = Black
                 | otherwise = Red

value :: Card -> Value
value (Card x _) = x

-- |Are the two cards alternate colours?
alternateColors :: Card -> Card -> Bool
alternateColors x y = color x /= color y

-- |Does the second card follow the first?
follows :: Card -> Card -> Bool
follows (Card King _) _ = False
follows (Card v1 _) (Card v2 _) = succ v1 == v2

data Card = Card Value Suit
            deriving (Eq,Show)

allCards :: [Card]
allCards = [Card x y | x <- [Ace .. King],  y <- [Clubs .. Spades]]
