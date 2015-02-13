module SuperMarket where

import Test.Hspec
import Test.QuickCheck

data Item = Apple

priceOf :: Item -> Int
priceOf = undefined

main :: IO ()
main = hspec $ do
  describe "Supermarket pricing" $ do
    it "returns the price of a single element" $ do
      priceOf Apple `shouldBe` 1
