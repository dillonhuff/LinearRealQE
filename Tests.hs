module Tests(main) where

import Test.Hspec

import Elimination
import Linear
import SignTable

main :: IO ()
main = hspec $ do
  describe "Possible order generation" $ do
    it "There is possible one order of one object" $ do
      length (allOrders [1]) `shouldBe` 1

    it "4 possible orders of 2 objects" $ do
      length (allOrders [1, 2]) `shouldBe` 4

  describe "Sign table construction" $ do
    it "Single expression table has one column" $ do
      numCols (tableForRootOrder (Value (mkLinear [(1, "x")] 3))) `shouldBe` 1
    it "Single expression table has three rows" $ do
      numRows (tableForRootOrder (Value (mkLinear [(1, "x")] 3))) `shouldBe` 3
      
