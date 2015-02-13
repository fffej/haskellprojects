{-

Coding kata taken from http://nimblepros.com/media/36760/supermarket%20pricing%20kata.pdf

-}
module SuperMarket where

import Test.Hspec
import Test.QuickCheck

data Item = Loaf

priceOf :: Item -> Int
priceOf _ = 1

main :: IO ()
main = hspec $ do
  describe "Supermarket pricing" $ do
    it "a loaf of bread is a dollar" $ do
      priceOf Loaf `shouldBe` 1
