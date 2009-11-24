module Klondike where

import Cards
import Data.Maybe
import Test.HUnit

data Tableau = Tableau 
    {
      a :: [Card]
    , b :: [Card]
    , c :: [Card]
    , d :: [Card]
    , e :: [Card]
    , f :: [Card]
    , g :: [Card]
    } deriving (Show)

tableauAsList :: Tableau -> [[Card]]
tableauAsList t = [(a t),(b t),(c t),(d t),(e t),(f t),(g t)]

tableauCards :: Tableau -> [Maybe Card]
tableauCards = map listToMaybe . tableauAsList

-- TODO how do I restrict the card to have a suit of the appropriate type
data Foundation = Foundation
    {
      clubs :: [Card]
    , diamonds :: [Card]
    , hearts :: [Card]
    , spades :: [Card]
    } deriving (Show)

emptyFoundation = Foundation [] [] [] []

data Game = Game 
    {
      deck :: [Card]
    , foundation :: Foundation
    , tableau :: Tableau
    } deriving (Show)

data Move = GameOver
          | TurnDeck
          | MoveFromDeck Card
            deriving (Show)

alternateColors :: Card -> Card -> Bool
alternateColors a b = color a /= color b

newGame :: [Card] -> Game
newGame cards = Game deck emptyFoundation tableu where
    (tableu,deck) = dealTableau cards

-- Must be a more compact way to do this (side effect clumsy)
dealTableau :: [Card] -> (Tableau,[Card])
dealTableau deck = (Tableau a b c d e f g,rest) where
    (a,h) = splitAt 1 deck
    (b,i) = splitAt 2 h
    (c,j) = splitAt 3 i
    (d,k) = splitAt 4 j
    (e,l) = splitAt 5 k
    (f,m) = splitAt 6 l
    (g,rest) = splitAt 7 m

-- Given a foundation card (maybe emtpy) and a potential card to move up
-- work out whether this can happen
-- TODO this is butt ugly
cardUp :: (Maybe Card) -> Card -> Bool
cardUp Nothing (Card Ace _) = True
cardUp Nothing (Card value _) = False
cardUp (Just x) (Card Ace _) = False
cardUp (Just (Card fvalue fsuit)) (Card value suit) = fsuit == suit && fvalue == pred value

-- TODO butt ugly again...
cardDown :: Card -> (Maybe Card) -> Bool
cardDown (Card King _) Nothing = True
cardDown _ Nothing = False
cardDown a@(Card x _) (Just b@(Card y _)) = alternateColors a b && succ x == y

-- TODO clarify
cardDownTableau :: Card -> Tableau -> [Card]
cardDownTableau c t = catMaybes $ filter (cardDown c) (tableauCards t)

-- TODO Clearly this is another lump of gibberish.  What's the right way?
getFoundationCards :: Foundation -> Card -> [Card]
getFoundationCards f (Card _ Clubs) = clubs f;
getFoundationCards f (Card _ Diamonds) = diamonds f;
getFoundationCards f (Card _ Hearts) = hearts f;
getFoundationCards f (Card _ Spades) = spades f;

moveUpTableau :: Foundation -> Card -> Maybe Suit
moveUpTableau f c@(Card Ace s) | null (getFoundationCards f c) = Just s
                               | otherwise = Nothing
moveUpTableau f c@(Card v s) | null (getFoundationCards f c) = Nothing
                             | value (head (getFoundationCards f c)) == pred v = Just s
    

turnDeck :: [Card] -> [Card]
turnDeck [] = []
turnDeck (x:xs) = xs ++ [x]

getMoves :: Game -> [Move]
getMoves game | canTurnDeck = [TurnDeck]  ++ cardDownMoves
              | otherwise = []
              where 
                t = tableau game
                f = foundation game
                canTurnDeck = (not . null . deck) game
                cardDownMoves = map MoveFromDeck (cardDownTableau ((head . deck) game) t)
                headTableau = (tableauCards t)
                

makeMove :: Game -> Move -> Game
makeMove g TurnDeck = undefined





          