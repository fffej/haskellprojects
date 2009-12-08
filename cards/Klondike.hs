module Klondike where

import Cards

import Data.List
import Data.Array
import Ix

-- |A type to index into the array
data Index = A | B | C | D | E | F | G 
           deriving (Eq, Ord, Show, Enum, Ix)

-- |A Slot is a pile of cards in the tableau, consisting of some shown cards and some hidden
data Slot = Slot 
    {
      shown :: [Card] 
    , hidden :: [Card] 
    }deriving (Show,Eq)

-- |The tableu is the seven possible piles of cards
type Tableau = Array Index Slot

-- |Moves determines the types of actions that can be taken
data Move = TurnDeck
          | ToFoundation Index 
          | DeckTo Index
          | DeckUp
          | MoveCards Index Int Index
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
newGame cards = Game d emptyFoundation t where
    (t,d) = dealTableau cards

empty :: Slot -> Bool
empty (Slot s h) = null s && null h

-- |Create empty foundation
emptyFoundation :: Foundation
emptyFoundation = Foundation (Base Spades []) (Base Clubs []) (Base Diamonds []) (Base Hearts [])

-- |Deal the tableau from a selection of cards.  Assumes that length [Card] 
-- is enough for this to succeed without error
dealTableau :: [Card] -> (Tableau,[Card])
dealTableau dk = ((array (A,G) [(A,(Slot [a] as))
                               ,(B,(Slot [b] bs))
                               ,(C,(Slot [c] cs))
                               ,(D,(Slot [d] ds))
                               ,(E,(Slot [e] es))
                               ,(F,(Slot [f] fs))
                               ,(G,(Slot [g] gs))]),
                  rest) where 
    (a:as,h) = splitAt 1 dk
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
cardDown (Card _ _) (Slot [] _) = False
cardDown a (Slot (b:_) _ ) = successor a b

-- |Can the card move to the given base?
cardUpFromDeck :: Card -> Base -> Bool
cardUpFromDeck (Card v s) (Base t []) = s == t && v == Ace 
cardUpFromDeck _ (Base _ (Card King _:_)) = False
cardUpFromDeck (Card v s) (Base t (Card x _:_)) = succ x == v && s == t

-- |Can we move up from the particular slot to a base object?
cardUpFromSlot :: Slot -> Base -> Bool
cardUpFromSlot (Slot x _) | null x = const False
                          | otherwise = cardUpFromDeck (head x)

-- |Lose a card from the given slot
dropCard :: Slot -> Slot
dropCard (Slot (_:[]) []) = Slot [] []
dropCard (Slot (_:[]) (y:ys)) = Slot [y] ys
dropCard (Slot (_:xs) y) = Slot xs y

dropCards :: Slot -> Int -> ([Card],Slot)
dropCards (Slot from []) n = (cards,(Slot r [])) where
    (cards,r) = splitAt n from
dropCards (Slot from (h:hs)) n = (cards,(Slot visible hidden)) where    
    (cards,x) = splitAt n from
    visible = if null x then [h] else x
    hidden = if null x then hs else h:hs

-- |Can the card move from x to y?
slotMove :: Slot -> Slot -> Bool
slotMove (Slot [] []) _ = False
slotMove (Slot (x:_) _) s = cardDown x s

-- |Turn the deck over
turnOverDeck [] = []
turnOverDeck (x:xs) = xs

move :: Game -> Move -> Game

move g GameOver = g

-- |Turn the deck, dropping the card from play
move g TurnDeck = Game (turnOverDeck (deck g)) (foundation g) (tableau g)

-- |Move the card from the deck to the foundation
move g DeckUp = Game (turnOverDeck (deck g)) f (tableau g) where
    f = addCard (head (deck g)) (foundation g)
    
-- |Move a card from the given slot to the foundation
move g (ToFoundation i) = Game (deck g) (addCard card f) t where
    f = foundation g
    slot = tableau g! i
    card = head $ shown slot
    t = tableau g // [(i,dropCard slot)] 


-- |Move a card from the deck to the given slot
move g (DeckTo i) = Game rest (foundation g) t where
    (c@(Card _ _):rest) = deck g
    (Slot s h) = tableau g ! i
    t = tableau g // [(i,Slot (c:s) h)]
        
-- |Move the given number of cards between two slots
move g (MoveCards fromIndex n toIndex) = Game (deck g) (foundation g) t where
    from = tableau g ! fromIndex
    (Slot tos hidden) = tableau g ! toIndex
    (newMove,updatedFrom) = dropCards from n
    updatedTo = Slot (newMove ++ tos) hidden
    t = tableau g // [(fromIndex,updatedFrom),(toIndex,updatedTo)]

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
updateTableau old new slots = s where
    s = fmap (\x -> if x == old then new else x) slots

-- |In the competition for uglist function that has ever existed there can be only one winner
-- And here it is...  (TODO make look less moronic)
addCard :: Card -> Foundation -> Foundation
addCard t@(Card _ s) 
        (Foundation w@(Base a as) x@(Base b bs) y@(Base c cs) z@(Base d ds)) 
            | s == a = Foundation (Base a (t:as)) x y z
            | s == b = Foundation w (Base b (t:bs)) y z
            | s == c = Foundation w x (Base c (t:cs)) z
            | s == d = Foundation w x y (Base d (t:ds))

-- TODO MoveCards
getMoves :: Game -> [Move]
getMoves g  = movesFromDeckToFoundation dk 
              ++ deckToSlot dk
              ++ turnDeckMove 
              ++ cardsUp 
              ++ slotMoves 
              ++ won where
    dk = deck g
    slots = assocs (tableau g)
    (Foundation s c d h) = foundation g
    won = [GameOver | all empty (map snd slots) && null dk]
    turnDeckMove = [TurnDeck | not.null $ dk]
    movesFromDeckToFoundation [] = []
    movesFromDeckToFoundation (x:xs) = [DeckUp | any (cardUpFromDeck x) [s,c,d,h]]
    cardsUp = undefined --concatMap (\base -> (map ToFoundation (filter (flip cardUpFromSlot base) slots))) [s,c,d,h]
    deckToSlot [] = []
    deckToSlot (z:ds) = map (DeckTo . fst) (filter (cardDown z . snd x) slots)
    slotMoves = undefined --[MoveCards x 1 y | x <- slots, y <- slots, slotMove x y]

-- |Play a game from the given state using the provider player function.  Get the list
-- of moves from the oringal state
playGame :: Game -> (Game -> [Move] -> Move) -> [Move]
playGame g player = nextMove : playGame nextGame player where
    moves = getMoves g
    nextMove = player g moves
    nextGame = move g nextMove

replayMoves :: Game -> [Move] -> Game
replayMoves = foldl move

-- |Selects the first available move.  This player gets into a loop slapping
-- items between any two slots
firstMove :: Game -> [Move] -> Move
firstMove g [] = GameOver
firstMove g (x:xs) = x

betterPlayer :: Game -> [Move] -> Move
betterPlayer g [] = GameOver
betterPlayer g m = head $ sortBy compareMoves m 
                                             
compareMoves :: Move -> Move -> Ordering
compareMoves a TurnDeck = LT
compareMoves _ DeckUp = GT
compareMoves _ (ToFoundation _) = GT
compareMoves _ (MoveCards _ _ _) = LT
compareMoves _ _ = EQ

