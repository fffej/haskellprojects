module Klondike where

import Cards
import Data.Maybe

-- |Identifiers for each possible "slot"
data Id = A | B | C | D | E | F | G deriving (Show,Eq)

-- |A Slot is a pile of cards in the tableau
data Slot = Slot [Card] Id deriving (Show,Eq)

-- |The Tableau is the seven possible piles of cards
-- I can't describe a fixed list of seven cards, but I won't export
-- this constructor and then all is well
data Tableau = Tableau [Slot] deriving (Show)

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
dealTableau deck = (Tableau [(Slot a A),(Slot b B),(Slot c C),(Slot d D),(Slot e E),(Slot f F),(Slot g G)]
                   ,rest) where
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
cardUpFromSlot (Slot (x:_) _) = cardUpFromDeck x

-- |Make the moves
move :: Game -> Move -> Game
move g TurnDeck = Game (turnDeck (deck g)) (foundation g) (tableau g) where 
    turnDeck [] = []
    turnDeck (x:xs) = xs ++ [x]

move g (ToFoundation (Slot (x:xs) id)) = Game d (addCard x f) t where
    d = deck g
    f = foundation g
    t = updateTableau (Slot xs id) (tableau g)

move g (DeckTo (Slot xs id)) = Game rest (foundation g) t where
    (c@(Card _ _):rest) = deck g
    t = updateTableau (Slot (c:xs) id) (tableau g) 
        
move g (MoveCards s i t) = undefined

move g (MoveCard (Slot (s:ss) from) (Slot x to)) = Game (deck g) (foundation g) t where
    u = updateTableau (Slot ss from) (tableau g)
    t = updateTableau (Slot (s:x) to) u

-- |Given an updated slot, update the create a new tableau reflecting this
updateTableau :: Slot -> Tableau -> Tableau
updateTableau s@(Slot _ i) t@(Tableau ss) = Tableau (map updateSlot ss) where
    updateSlot a@(Slot _ j) = if j==i then s else a

-- |In the competition for uglist function that has ever existed there can be only one winner
-- And here it is...  (TODO make look less moronic)
addCard :: Card -> Foundation -> Foundation
addCard t@(Card _ s) 
        (Foundation w@(Base a as) x@(Base b bs) y@(Base c cs) z@(Base d ds)) 
            | s == a = Foundation (Base a (t:as)) x y z
            | s == b = Foundation w (Base b (t:bs)) y z
            | s == c = Foundation w x (Base c (t:cs)) z
            | s == d = Foundation w x y (Base d (t:ds))
