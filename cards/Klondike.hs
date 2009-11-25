module Klondike where

import Cards
import Data.Maybe

-- |Identifiers for each possible "slot"
data Id = A | B | C | D | E | F | G deriving (Show,Eq)

-- |A Slot is a pile of cards in the tableau
data Slot = Slot [Card] Id deriving (Show,Eq)

-- |The Tableau is the seven possible piles of cards
data Tableau = Tableau Slot Slot Slot Slot Slot Slot Slot deriving (Show)

-- |Moves determines the types of actions that can be taken
data Move = TurnDeck
          | ToFoundation Slot 
          | DeckTo Slot
          | MoveCards Slot Int Slot 
          | MoveCard Slot Slot
          | GameOver
            deriving (Show,Eq)

data Base = Base Suit [Card] deriving (Eq,Show)
data Foundation = Foundation Base Base Base Base 
                  deriving (Show)

-- |A game consists of a single deck of cards, a foundation and a tableau
data Game = Game 
    {
      deck :: [Card]
    , foundation :: Foundation
    , tableau :: Tableau
    } deriving (Show)

-- |Create a new game from the given deck of cards (assumed a full deck)
newGame :: [Card] -> Game
newGame cards = Game deck emptyFoundation tableu where
    (tableu,deck) = dealTableau cards

-- |Create empty foundation
emptyFoundation = Foundation (Base Spades []) (Base Clubs []) (Base Diamonds []) (Base Hearts [])

-- |Deal the tableau from a selection of cards.  Assumes that length [Card] 
-- is enough for this to succeed without error
dealTableau :: [Card] -> (Tableau,[Card])
dealTableau deck = (Tableau 
                     (Slot a A) (Slot b B) (Slot c C) 
                     (Slot d D) (Slot e E) (Slot f F) (Slot g G),
                     rest) where
    (a,h) = splitAt 1 deck
    (b,i) = splitAt 2 h
    (c,j) = splitAt 3 i
    (d,k) = splitAt 4 j
    (e,l) = splitAt 5 k
    (f,m) = splitAt 6 l
    (g,rest) = splitAt 7 m

-- |Can the card move down from the deck to the given slot?
cardDown :: Card -> Slot -> Bool
cardDown (Card King _) (Slot [] _) = True
cardDown (Card King s) (Slot (x:xs) _) = False
cardDown a@(Card x _) (Slot (b@(Card y _):xs) _) = alternateColors a b && succ x == y

-- |Can the card move to the given base?
cardUpFromDeck :: Card -> Base -> Bool
cardUpFromDeck (Card v s) (Base t []) = s == t && v == Ace 
cardUpFromDeck (Card v s) (Base t (Card King b:xs)) = False
cardUpFromDeck (Card v s) (Base t (Card x b:xs)) = succ x == v && s == t

-- |Can we move up from the particular slot to a base object?
cardUpFromSlot :: Slot -> Base -> Bool
cardUpFromSlot (Slot (x:xs) _) b = cardUpFromDeck x b

-- | 

{-

-- TODO This should return the target card NOT the source card.  Should be list [Maybe Card]
-- as a return type
-- Where can we move the given card on the tableau?
cardDownTableau :: Card -> Tableau -> [Card]
cardDownTableau c = catMaybes . filter (cardDown c) . tableauCards

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
-}          