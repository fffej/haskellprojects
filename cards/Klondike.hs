module Klondike where

import Cards

import Data.Maybe
import Data.List

-- |Identifiers for each possible "slot"
data Id = A | B | C | D | E | F | G deriving (Show,Eq)

-- |A Slot is a pile of cards in the tableau, consisting of some shown cards and some hidden
data Slot = Slot 
    {
      shown :: [Card] 
    , hidden :: [Card] 
    , id :: Id 
    }deriving (Show,Eq)

-- |The Tableau is the seven possible piles of cards
-- I can't describe a fixed list of seven cards, but I won't export
-- this constructor and then all is well
data Tableau = Tableau [Slot] deriving (Show)

-- |Moves determines the types of actions that can be taken
data Move = TurnDeck
          | ToFoundation Slot 
          | DeckTo Slot
          | DeckUp
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
dealTableau deck = (Tableau [
                     (Slot [a] as A),
                     (Slot [b] bs B),
                     (Slot [c] cs C),
                     (Slot [d] ds D),
                     (Slot [e] es E),
                     (Slot [f] fs F),
                     (Slot [g] gs G)]
                   ,rest) where
    (a:as,h) = splitAt 1 deck
    (b:bs,i) = splitAt 2 h
    (c:cs,j) = splitAt 3 i
    (d:ds,k) = splitAt 4 j
    (e:es,l) = splitAt 5 k
    (f:fs,m) = splitAt 6 l
    (g:gs,rest) = splitAt 7 m

-- |Does the second card follow the first?
successor :: Card -> Card -> Bool
successor (Card King _) _ = False
successor a b = alternateColors a b && follows a b

-- |Can the card move down from the deck to the given slot?
cardDown :: Card -> Slot -> Bool
cardDown (Card King _) (Slot [] _ _) = True
cardDown a@(Card x _) (Slot (b@(Card y _):xs) _ _) = successor a b

-- |Can the card move to the given base?
cardUpFromDeck :: Card -> Base -> Bool
cardUpFromDeck (Card v s) (Base t []) = s == t && v == Ace 
cardUpFromDeck (Card v s) (Base t (Card King b:xs)) = False
cardUpFromDeck (Card v s) (Base t (Card x b:xs)) = succ x == v && s == t

-- |Can we move up from the particular slot to a base object?
cardUpFromSlot :: Slot -> Base -> Bool
cardUpFromSlot (Slot (x:_) _ _) = cardUpFromDeck x

-- |Lose a card from the given slot
dropCard :: Slot -> Slot
dropCard (Slot (x:[]) [] id) = Slot [] [] id
dropCard (Slot (x:[]) (y:ys) id) = Slot [y] ys id
dropCard (Slot (x:xs) y id) = Slot xs y id

dropCards :: Slot -> Int -> ([Card],Slot)
dropCards (Slot from [] id) n = (cards,(Slot r [] id)) where
    (cards,r) = splitAt n from
dropCards (Slot from (h:hs) id) n = (cards,(Slot visible hidden id)) where    
    (cards,x) = splitAt n from
    visible = if null x then [h] else x
    hidden = if null x then hs else h:hs

move :: Game -> Move -> Game

-- |Turn the deck
move g TurnDeck = Game (turnDeck (deck g)) (foundation g) (tableau g) where 
    turnDeck [] = []
    turnDeck (x:xs) = xs ++ [x]

-- |Move a card from the given slot to the foundation
move g (ToFoundation s@(Slot (x:xs) _ _)) = Game d (addCard x f) t where
    d = deck g
    f = foundation g
    t = updateTableau (dropCard s) (tableau g)

-- |Move a card from the deck to the given slot
move g (DeckTo (Slot xs ys id)) = Game rest (foundation g) t where
    (c@(Card _ _):rest) = deck g
    t = updateTableau (Slot (c:xs) ys id) (tableau g) 
        
-- |Move a single card between two slots
move g (MoveCard from@(Slot (s:ss) _ fid) (Slot x h to)) = Game (deck g) (foundation g) t where
    u = updateTableau (dropCard from) (tableau g)
    t = updateTableau (Slot (s:x) h to) u

-- |Move the given number of cards between two slots
move g (MoveCards s n (Slot tos hidden to)) = Game (deck g) (foundation g) t where
    (move,updatedFrom) = dropCards s n
    updatedTo = Slot (move ++ tos) hidden to
    t = updateTableau updatedTo (updateTableau updatedFrom (tableau g))

-- |Given the stack of cards, find the longest sequence of cards
consecutiveCards :: [Card] -> [Card]
consecutiveCards [] = []
consecutiveCards (x:[]) = [x]
consecutiveCards (x:y:xs) | successor x y = x : consecutiveCards (y:xs)
                          | otherwise = [x]

-- |Given a series of consecutive cards, produce all the initial occurrences
-- TODO less bizarre implementation!
heads :: [a] -> [[a]]
heads = init . map reverse . (tails . reverse)

-- |Given an updated slot, update the create a new tableau reflecting this
updateTableau :: Slot -> Tableau -> Tableau
updateTableau s@(Slot _ _ i) t@(Tableau ss) = Tableau (map updateSlot ss) where
    updateSlot a@(Slot _ _ j) = if j==i then s else a

-- |In the competition for uglist function that has ever existed there can be only one winner
-- And here it is...  (TODO make look less moronic)
addCard :: Card -> Foundation -> Foundation
addCard t@(Card _ s) 
        (Foundation w@(Base a as) x@(Base b bs) y@(Base c cs) z@(Base d ds)) 
            | s == a = Foundation (Base a (t:as)) x y z
            | s == b = Foundation w (Base b (t:bs)) y z
            | s == c = Foundation w x (Base c (t:cs)) z
            | s == d = Foundation w x y (Base d (t:ds))

getMoves :: Game -> [Move]
getMoves g  = movesFromDeckToFoundation dk ++ turnDeckMove where
    dk = deck g
    (Tableau slots) = tableau g
    (Foundation s c d h) = foundation g
    turnDeckMove = if null dk then [] else [TurnDeck]
    movesFromDeckToFoundation [] = []
    movesFromDeckToFoundation (x:xs) = [DeckUp | any (cardUpFromDeck x) [s,c,d,h]]
                                             
    
                 