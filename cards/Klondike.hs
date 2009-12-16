module Klondike where

import Cards

import Data.List
import Data.Array
import Ix

import Random

-- |A type to index into the array
data Index = A | B | C | D | E | F | G  deriving (Eq, Ord, Show, Enum, Ix)

-- |A Slot is a pile of cards in the tableau, consisting of some shown cards and some hidden
data Slot = Slot 
    {
      shown :: [Card] 
    , hidden :: [Card] 
    } deriving (Show,Eq)

dropCard :: Slot -> Slot
dropCard (Slot s h) = Slot (f ++ drop 1 s) r where
    (f,r) = splitAt 1 h

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

-- |A foundation is a collection of bases
type Foundation = Array Suit [Card]

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


-- |Create empty foundation
emptyFoundation :: Foundation
emptyFoundation = array (Clubs,Spades) [(s,[]) | s <- [Clubs .. Spades]]

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


-- |Can the card move down from the deck to the given slot?
cardDown :: Card -> Slot -> Bool
cardDown card (Slot s _) | null s = value card == King
                         | otherwise = successor card (head s)

-- |Can the card move to foundation?
cardUp :: Card -> Foundation -> Bool 
cardUp (Card v suit) f | null cards = v == Ace
                       | otherwise = x /=King && succ x == v 
                            where
                              cards = f ! suit
                              (Card x _) = head cards


-- |Can we move up from the particular slot to a base object?
cardUpFromSlot :: Slot -> Foundation -> Bool
cardUpFromSlot (Slot x _) | null x = const False
                          | otherwise = cardUp (head x)

dropCards :: Slot -> Int -> ([Card],Slot)
dropCards (Slot from []) n = (cards,(Slot r [])) where
    (cards,r) = splitAt n from
dropCards (Slot from (h:hs)) n = (cards,(Slot visible hdn)) where    
    (cards,x) = splitAt n from
    visible = if null x then [h] else x
    hdn = if null x then hs else h:hs

-- |Can the card move from x to y?
slotMove :: Slot -> Slot -> Bool
slotMove (Slot from _) s | null from = False
                         | otherwise = cardDown (head from) s

-- |Turn the deck over
turnOverDeck :: [Card] -> [Card]
turnOverDeck [] = []
turnOverDeck (_:xs) = xs

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
    (Slot tos hdn) = tableau g ! toIndex
    (newMove,updatedFrom) = dropCards from n
    updatedTo = Slot (newMove ++ tos) hdn
    t = tableau g // [(fromIndex,updatedFrom),(toIndex,updatedTo)]

-- |Given the stack of cards, find the longest sequence of cards
consecutiveCards :: [Card] -> [Card]
consecutiveCards [] = []
consecutiveCards (x:[]) = [x]
consecutiveCards (x:y:xs) | successor x y = x : consecutiveCards (y:xs)
                          | otherwise = [x]

-- |Add the given card to the foundation
addCard :: Card -> Foundation -> Foundation
addCard t@(Card _ s) f = f // [(s,(t:f!s))]

-- |Is the game complete?
gameWon :: Game -> Bool
gameWon game = all empty (elems (tableau game)) && not (null (deck game)) where
    empty (Slot s h) = null s && null h

-- TODO Move more than 1 card at a time
getMoves :: Game -> [Move]
getMoves g  = [DeckUp | (not.null) dk && cardUp (head dk) (foundation g)]
              ++ [(DeckTo . fst) x | not.null $ dk, x <- filter (cardDown (head dk) . snd) slots]
              ++ [TurnDeck | not.null $ dk] 
              ++ [ToFoundation is | is <- cardsUp]
              ++ slotMoves 
              ++ [GameOver | gameWon g] where
    dk = deck g
    slots = assocs (tableau g)
    cardsUp = map fst (filter (flip cardUpFromSlot (foundation g) . snd) slots)
    slotMoves = [MoveCards (fst x) 1 (fst y) | x <- slots, y <- slots, 
                                               fst x /= fst y && slotMove (snd x) (snd y)]

-- |Play a game from the given state using the provider player function.  Get the list
-- of moves from the original state
playGame :: Game -> (Game -> [Move] -> Move) -> [Move]
playGame g p = nextMove : playGame nextGame p where
    nextMove = p g (getMoves g)
    nextGame = move g nextMove

replayMoves :: Game -> [Move] -> Game
replayMoves = foldl move

player :: Game -> [Move] -> Move
player _ [] = GameOver
player _ m = head $ sortBy compareMoves m 
                                             
compareMoves :: Move -> Move -> Ordering
compareMoves _ TurnDeck = LT
compareMoves _ DeckUp = GT
compareMoves _ (ToFoundation _) = GT
compareMoves _ (MoveCards _ _ _) = LT
compareMoves _ _ = EQ

-- Code nabbed from http://aoeu.snth.net/?p=119
shuffle :: (RandomGen g) => g -> [Card]
shuffle g = fst (mix allCards (randomRs (True, False) g))
    where mix [ ] r0 = ([ ], r0)
          mix [x] r0 = ([x], r0)
          mix  xs r0 = let (ys, zs, r1) = cut xs r0 [] []
                           (cs,     r2) = mix ys r1
                           (ds,     r3) = mix zs r2
                       in  (cs++ds, r3)
          cut    []     rs  ys zs = (ys, zs, rs)
          cut (x:xs) (r:rs) ys zs = if r then cut xs rs (x:ys) zs
                                         else cut xs rs ys (x:zs)


-- Infinite list of random games
randomGames :: [Game]
randomGames = map (\x -> newGame (shuffle (mkStdGen x))) [1..]

-- TODO this currently blows up because I get into an infinite loop
-- MoveCards B 1 G,MoveCards G 1 B repeated over and over again
foo = map (takeWhile (/= GameOver)) (map ((flip playGame) player) (take 100 randomGames))



  
