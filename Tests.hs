module Tests(main) where

import Test.Hspec

import Linear

main :: IO ()
main = hspec $ do
  describe "Possible order generation" $ do
    it "There is possible one order of one object" $ do
      length (allOrders [1]) `shouldBe` 1

    it "4 possible orders of 2 objects" $ do
      length (allOrders [1, 2]) `shouldBe` 4
