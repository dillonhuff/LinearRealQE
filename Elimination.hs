module Elimination where

import Linear
import SignTable

middleIntervals :: [String] -> [Interval]
middleIntervals [x] = [Point $ Var x]
middleIntervals (x:y:xs) =
  [Point $ Var x, Range (Var x) (Var y)] ++ (middleIntervals (y:xs))

buildIntervals :: [LinearExpr] -> [Interval]
buildIntervals lx =
  let vars = map (\v -> "x" ++ show v) $ [1..(length lx)]
      negInf = Range NInf (Var $ head vars)
      pInf = Range (Var $ last vars) Inf in
   (negInf:(middleIntervals vars)) ++ [pInf]
   

tableForRootOrder :: Order LinearExpr -> SignTable
tableForRootOrder order =
  let es = extractElems order in
   mkTable es (buildIntervals es) []
