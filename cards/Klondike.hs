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

tableauCards :: Tableau -> [Maybe Card]
tableauCards = map listToMaybe . tableauAsList where
    tableauAsList t = [(a t),(b t),(c t),(d t),(e t),(f t),(g t)]

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
          | MoveUp Card
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

-- Given a foundation card (maybe empty) and a potential card to move up can we?
cardUp :: (Maybe Card) -> Card -> Bool
cardUp Nothing (Card Ace _) = True
cardUp Nothing (Card value _) = False
cardUp (Just x) (Card Ace _) = False
cardUp (Just (Card fvalue fsuit)) (Card value suit) = fsuit == suit && fvalue == pred value

-- Given a card and a potential target on the tableau (might be empty), can we move it down?
cardDown :: Card -> (Maybe Card) -> Bool
cardDown (Card King _) Nothing = True
cardDown _ Nothing = False
cardDown a@(Card x _) (Just b@(Card y _)) = alternateColors a b && succ x == y

-- TODO This should return the target card NOT the source card.  Should be list [Maybe Card]
-- as a return type
-- Where can we move the given card on the tableau?
cardDownTableau :: Card -> Tableau -> [Card]
cardDownTableau c = catMaybes . filter (cardDown c) . tableauCards

-- TODO Clearly this is another lump of gibberish.  What's the right way?
getSuit :: Foundation -> Card -> [Card]
getSuit f (Card _ Clubs) = clubs f;
getSuit f (Card _ Diamonds) = diamonds f;
getSuit f (Card _ Hearts) = hearts f;
getSuit f (Card _ Spades) = spades f;

moveUpTableau :: Foundation -> Card -> Maybe Card
moveUpTableau f c@(Card Ace _) | null (getSuit f c) = Just c
                               | otherwise = Nothing
moveUpTableau f c@(Card v _) | null (getSuit f c) = Nothing
                             | value (head (getSuit f c)) == pred v = Just c

turnDeck :: [Card] -> [Card]
turnDeck [] = []
turnDeck (x:xs) = xs ++ [x]

-- A list of available moves that can be played from the current position
getMoves :: Game -> [Move]
getMoves game | canTurnDeck = TurnDeck : cardDownMoves ++ cardUpMoves
              | otherwise = cardDownMoves ++ cardUpMoves
              where 
                t = tableau game
                f = foundation game
                ts = (catMaybes . tableauCards) t
                canTurnDeck = (not . null . deck) game
                cardDownMoves = map MoveFromDeck (cardDownTableau ((head . deck) game) t)
                cardUpMoves = map MoveUp (catMaybes (map (moveUpTableau f) ts))

makeMove :: Game -> Move -> Game
makeMove g TurnDeck = Game (turnDeck (deck g)) (foundation g) (tableau g)
makeMove g (MoveFromDeck c) = undefined
makeMove g (MoveUp c) = undefined



          