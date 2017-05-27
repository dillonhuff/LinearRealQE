module Linear where

import Data.Maybe
import Data.List

data LinearExpr = LinearExpr [(Rational, String)] Rational
                  deriving (Eq, Ord, Show)


sumCoeffs :: [(Rational, String)] -> (Rational, String)
sumCoeffs [] = error "sumCoeffs empty"
sumCoeffs cs =
  let v = snd $ head cs in
   (foldr (+) 0 $ map fst cs, v)

addLikeTerms a =
  let grouped = groupBy (\(_, a) (_, b) -> a == b)$ sortBy (\(_, b) (_, d) -> compare b d) a in
   map sumCoeffs grouped

mkLinear terms const = LinearExpr (filter (\(a, _) -> a /= 0) $ addLikeTerms terms) const

scalarTimes :: Rational -> LinearExpr -> LinearExpr
scalarTimes r (LinearExpr terms c) =
  mkLinear (map (\(a, b) -> (r*a, b)) terms) (r*c)

getTerm :: String -> LinearExpr -> (Rational, String)
getTerm varName (LinearExpr terms _) =
  fromJust $ find (\(a, b) -> b == varName) terms

   
plus :: LinearExpr -> LinearExpr -> LinearExpr
plus (LinearExpr a c) (LinearExpr q r) =
  let newC = c - r
      newTerms = a ++ q in
   mkLinear newTerms newC

minus a b = plus a (scalarTimes (-1) b)

symRoot :: String -> LinearExpr -> LinearExpr
symRoot varName linExpr =
  let term = getTerm varName linExpr in
   scalarTimes (-1) $ scalarTimes (1 / (fst term)) $ minus linExpr (mkLinear [term] 0)
