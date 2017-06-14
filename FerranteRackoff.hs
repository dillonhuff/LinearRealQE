module FerranteRackoff where

import Data.List

import Elimination
import Linear
import Logic

subTerm varName e a =
  let t = fst $ getTerm varName a
      aWithoutVar = minus a (mkLinear [(t, varName)] 0) in
   plus e $ aWithoutVar

substituteForVar :: String -> LinearExpr -> Formula LinearExpr -> Formula LinearExpr
substituteForVar varName e (And a b) =
  And (substituteForVar varName e a) (substituteForVar varName e b)
substituteForVar varName e (Or a b) =
  Or (substituteForVar varName e a) (substituteForVar varName e b)
substituteForVar varName e (Not a) =
  Not (substituteForVar varName e a)
substituteForVar varName e (Atom comp a) =
  Atom comp $ subTerm varName e a
substituteForVar _ _ f = f

buildFRTestPoints :: String -> [LinearExpr] -> [LinearExpr]
buildFRTestPoints varName fs =
  let symRoots = map (symRoot varName) fs
      -- Need to filter out identical cases
      midPoints = [scalarTimes (1 / 2) (plus a b) | a <- symRoots, b <- symRoots]
      one = mkLinear [] 1
      negInfPoints = map (\a -> minus a one) symRoots 
      posInfPoints = map (plus one) symRoots in
   nub $ symRoots ++ midPoints ++ negInfPoints ++ posInfPoints

ferranteRackoff :: String -> Formula LinearExpr -> Formula LinearExpr
ferranteRackoff s f =
  let fs = nub $ collectFormulas f
      testPoints = buildFRTestPoints s fs in
   simplifyFmRec $ foldOrs $ map (\a -> substituteForVar s a f) testPoints
