module Klondike where

import Cards

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
data Move = TurnDeck
          | ToFoundation Slot 
          | DeckTo Slot
          | DeckUp
          | MoveCards Slot Int Slot
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
dealTableau dk = (Tableau [
                   (Slot [a] as),
                   (Slot [b] bs),
                   (Slot [c] cs),
                   (Slot [d] ds),
                   (Slot [e] es),
                   (Slot [f] fs),
                   (Slot [g] gs)]
                 ,rest) where
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
move g (ToFoundation s@(Slot (x:xs) _)) = Game d (addCard x f) t where
    d = deck g
    f = foundation g
    t = updateTableau s (dropCard s) (tableau g)

-- |Move a card from the deck to the given slot
move g (DeckTo old@(Slot xs ys)) = Game rest (foundation g) t where
    (c@(Card _ _):rest) = deck g
    t = updateTableau old (Slot (c:xs) ys) (tableau g) 
        
-- |Move the given number of cards between two slots
move g (MoveCards from n to@(Slot tos h)) = Game (deck g) (foundation g) t where
    (newMove,updatedFrom) = dropCards from n
    updatedTo = Slot (newMove ++ tos) h
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
updateTableau old new (Tableau slots) = Tableau s where
    s = map (\x -> if x == old then new else x) slots

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
    (Tableau slots) = tableau g
    (Foundation s c d h) = foundation g
    won = [GameOver | all empty slots && null dk]
    turnDeckMove = [TurnDeck | not.null $ dk]
    movesFromDeckToFoundation [] = []
    movesFromDeckToFoundation (x:xs) = [DeckUp | any (cardUpFromDeck x) [s,c,d,h]]
    cardsUp = concatMap (\base -> (map ToFoundation (filter (flip cardUpFromSlot base) slots))) [s,c,d,h]
    deckToSlot [] = []
    deckToSlot (z:ds) = map DeckTo (filter (cardDown z) slots)
    slotMoves = [MoveCards x 1 y | x <- slots, y <- slots, slotMove x y]

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

{-

Comment from Blog

At a guess, you are probably not using the Suit argument to the Base type. (Or, at least, not in a critical way.)

Here's an idea of a way to ensure that there at most seven slots in the Tableau: define a new enumeration type

data Index = One | Two | ... | Seven deriving (Eq, Ord, Show, Read, Enum, Ix)

Then let your Tableau be an array using Index as its index type.

To guarantee that shown cards alternate color, you could use phantom types. For example

data RED
data BLACK
data Shown = ShownRed RedList | ShownBlack BlackList
data RedList = NilRed | ConsRed (Card RED) BlackList
data BlackList = NilBlack | ConsBlack (Card BLACK) RedList

Then you need to define a new Card type that has a type argument, and only expose smart constructors like

club, spade :: CardNumber -> Card BLACK
diamond, heart :: CardNumber -> Card RED

I'm sure this will all get horribly mangled; I apologize in advance.

-}