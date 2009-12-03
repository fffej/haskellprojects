module Klondike where

import Cards

import Data.Maybe
import Data.List

-- |A Slot is a pile of cards in the tableau, consisting of some shown cards and some hidden
data Slot = Slot 
    {
      shown :: [Card] 
    , hidden :: [Card] 
    }deriving (Show,Eq)

-- |The Tableau is the seven possible piles of cards
-- I can't describe a fixed list of seven cards, but I won't export
-- this constructor and then all is well
data Tableau = Tableau [Slot] deriving (Show)

-- |Moves determines the types of actions that can be taken
-- TODO consolidate moveCard and move cards
data Move = TurnDeck
          | ToFoundation Slot 
          | DeckTo Slot
          | DeckUp
          | MoveCards Slot Int Slot 
          | MoveCard Slot Slot
          | GameOver
            deriving (Show,Eq)

-- |A base is based on a particular suit of cards
data Base = Base Suit [Card] deriving (Eq,Show)

-- |A foundation is a collection of bases
-- TODO Can I specify that they must have different suits?
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

empty :: Slot -> Bool
empty (Slot s h) = null s && null h

-- |Create empty foundation
emptyFoundation = Foundation (Base Spades []) (Base Clubs []) (Base Diamonds []) (Base Hearts [])

-- |Deal the tableau from a selection of cards.  Assumes that length [Card] 
-- is enough for this to succeed without error
dealTableau :: [Card] -> (Tableau,[Card])
dealTableau deck = (Tableau [
                     (Slot [a] as),
                     (Slot [b] bs),
                     (Slot [c] cs),
                     (Slot [d] ds),
                     (Slot [e] es),
                     (Slot [f] fs),
                     (Slot [g] gs)]
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
cardDown (Card King _) (Slot [] _ ) = True
cardDown a@(Card x _) (Slot (b@(Card y _):xs) _ ) = successor a b

-- |Can the card move to the given base?
cardUpFromDeck :: Card -> Base -> Bool
cardUpFromDeck (Card v s) (Base t []) = s == t && v == Ace 
cardUpFromDeck (Card v s) (Base t (Card King b:xs)) = False
cardUpFromDeck (Card v s) (Base t (Card x b:xs)) = succ x == v && s == t

-- |Can we move up from the particular slot to a base object?
cardUpFromSlot :: Slot -> Base -> Bool
cardUpFromSlot (Slot (x:_) _ ) = cardUpFromDeck x

-- |Lose a card from the given slot
dropCard :: Slot -> Slot
dropCard (Slot (x:[]) []) = Slot [] []
dropCard (Slot (x:[]) (y:ys)) = Slot [y] ys
dropCard (Slot (x:xs) y) = Slot xs y

dropCards :: Slot -> Int -> ([Card],Slot)
dropCards (Slot from []) n = (cards,(Slot r [])) where
    (cards,r) = splitAt n from
dropCards (Slot from (h:hs)) n = (cards,(Slot visible hidden)) where    
    (cards,x) = splitAt n from
    visible = if null x then [h] else x
    hidden = if null x then hs else h:hs

-- |Can the card move from x to y?
slotMove :: Slot -> Slot -> Bool
slotMove (Slot [] []) (Slot [] []) = False
slotMove (Slot (x:xs) _) s = cardDown x s

move :: Game -> Move -> Game

-- |Turn the deck
move g TurnDeck = Game (turnDeck (deck g)) (foundation g) (tableau g) where 
    turnDeck [] = []
    turnDeck (x:xs) = xs ++ [x]

-- |Move a card from the given slot to the foundation
move g (ToFoundation s@(Slot (x:xs) _)) = Game d (addCard x f) t where
    d = deck g
    f = foundation g
    t = updateTableau s (dropCard s) (tableau g)

-- |Move a card from the deck to the given slot
move g (DeckTo old@(Slot xs ys)) = Game rest (foundation g) t where
    (c@(Card _ _):rest) = deck g
    t = updateTableau old (Slot (c:xs) ys) (tableau g) 
        
-- |Move a single card between two slots
move g (MoveCard from@(Slot (s:ss) _) to@(Slot x h)) = Game (deck g) (foundation g) t where
    u = updateTableau from (dropCard from) (tableau g)
    t = updateTableau to (Slot (s:x) h) u

-- |Move the given number of cards between two slots
move g (MoveCards from n to@(Slot tos hidden)) = Game (deck g) (foundation g) t where
    (move,updatedFrom) = dropCards from n
    updatedTo = Slot (move ++ tos) hidden
    t = updateTableau to updatedTo (updateTableau from updatedFrom (tableau g))

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
updateTableau :: Slot -> Slot -> Tableau -> Tableau
updateTableau old new tableau = tableau

-- |In the competition for uglist function that has ever existed there can be only one winner
-- And here it is...  (TODO make look less moronic)
addCard :: Card -> Foundation -> Foundation
addCard t@(Card _ s) 
        (Foundation w@(Base a as) x@(Base b bs) y@(Base c cs) z@(Base d ds)) 
            | s == a = Foundation (Base a (t:as)) x y z
            | s == b = Foundation w (Base b (t:bs)) y z
            | s == c = Foundation w x (Base c (t:cs)) z
            | s == d = Foundation w x y (Base d (t:ds))

-- TurnDeck ToFoundation DeckTo DeckUp
-- TODO MoveCards
getMoves :: Game -> [Move]
getMoves g  = movesFromDeckToFoundation dk 
              ++ deckToSlot dk
              ++ turnDeckMove 
              ++ cardsUp 
              ++ slotMoves 
              ++ won where
    dk = deck g
    (Tableau slots) = tableau g
    (Foundation s c d h) = foundation g
    won = [GameOver | all empty slots && null dk]
    turnDeckMove = [TurnDeck | not.null $ dk]
    movesFromDeckToFoundation [] = []
    movesFromDeckToFoundation (x:xs) = [DeckUp | any (cardUpFromDeck x) [s,c,d,h]]
    cardsUp = concatMap (\base -> (map ToFoundation (filter (flip cardUpFromSlot base) slots))) [s,c,d,h]
    deckToSlot [] = []
    deckToSlot (d:ds) = map DeckTo (filter (cardDown d) slots)
    slotMoves = [MoveCard x y | x <- slots, y <- slots, slotMove x y]
                                             
