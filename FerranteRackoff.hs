module FerranteRackoff where

import Data.List

import Elimination
import Linear
import Logic

substituteForVar :: String -> LinearExpr -> Formula LinearExpr -> Formula LinearExpr
substituteForVar varName e f = f

buildFRTestPoints :: String -> [LinearExpr] -> [LinearExpr]
buildFRTestPoints varName fs =
  let symRoots = map (symRoot varName) fs
      -- Need to filter out identical cases
      midPoints = [scalarTimes (1 / 2) (plus a b) | a <- symRoots, b <- symRoots]
      one = mkLinear [] 1
      negInfPoints = map (minus one) symRoots 
      posInfPoints = map (plus one) symRoots in
   symRoots ++ midPoints ++ negInfPoints ++ posInfPoints

ferranteRackoff :: String -> Formula LinearExpr -> Formula LinearExpr
ferranteRackoff s f =
  let fs = nub $ collectFormulas f
      testPoints = buildFRTestPoints s fs in
   foldOrs $ map (\a -> substituteForVar s a f) testPoints
