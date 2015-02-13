{-

Coding kata taken from http://nimblepros.com/media/36760/supermarket%20pricing%20kata.pdf

-}
module SuperMarket where

import Test.Hspec
import Test.QuickCheck

data Money = Cents Integer deriving (Show,Eq)

dollar :: Integer -> Money
dollar x = Cents (x * 100)

cents :: Integer -> Money
cents = Cents 

data Item = Loaf
          | Noodles
          | Soup

priceOf :: Item -> Money
priceOf Loaf    = dollar 1
priceOf Noodles = cents 50
priceOf Soup    = dollar 2 

main :: IO ()
main = hspec $ do
  describe "Supermarket pricing" $ do
    it "a loaf of bread is a dollar" $ do
      priceOf Loaf `shouldBe` (Cents 100)
    it "a pack of noodles is 50 cents" $ do
      priceOf Noodles `shouldBe` (Cents 50)
    it "a can of soup is 2 dollars" $ do
      priceOf Soup `shouldBe` (Cents 200)
