module Tests(main) where

import Test.Hspec

import Elimination
import Linear
import Logic
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

    it "Two expression table has five rows" $ do
      numRows (tableForRootOrder (Less (mkLinear [(-2, "x")] 0) $ Value (mkLinear [(1, "x")] 3))) `shouldBe` 5

    describe "Evaluate formula with sign table" $ do
      it "x = 0 has sat assignment" $ do
        let x = mkLinear [(1, "x")] 0
            st = mkTable [x] [Range NInf (Var "x0"), Point $ Var "x0", Range (Var "x0") Inf] [[Neg], [Zero], [Pos]] in
         formulaIsSAT st (Atom EQL x) `shouldBe` True

      it "x = 0 and x > 0 does not have sat assignment" $ do
        let x = mkLinear [(1, "x")] 0
            fm = And (Atom EQL x) (Atom GREATER x)
            st = mkTable [x] [Range NInf (Var "x0"), Point $ Var "x0", Range (Var "x0") Inf] [[Neg], [Zero], [Pos]] in
         formulaIsSAT st fm `shouldBe` False
