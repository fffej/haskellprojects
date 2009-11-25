module KlondikeTest where

import Klondike
import Cards
import Test.HUnit

testCardDownTableau = TestCase (assertEqual "Moves are correct" [Card Two Hearts] f)
    where
      tableau = Tableau [Card Two Hearts] [] [] [] [] [] []
      f = cardDownTableau (Card Ace Spades) tableau
