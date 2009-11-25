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
color (Card _ Clubs) = Black
color (Card _ Diamonds) = Red
color (Card _ Hearts) = Red
color (Card _ Spades) = Black

value :: Card -> Value
value (Card x _) = x

alternateColors :: Card -> Card -> Bool
alternateColors a b = color a /= color b

data Card = Card Value Suit
            deriving (Eq,Show)

allCards :: [Card]
allCards = [Card x y | x <- [Ace .. King],  y <- [Clubs .. Spades]]

