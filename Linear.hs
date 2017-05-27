module Linear where

import Data.Maybe
import Data.List

data LinearExpr = LinearExpr [(Rational, String)] Rational
                  deriving (Eq, Ord, Show)

mkLinear terms const = LinearExpr terms const

scalarTimes :: Rational -> LinearExpr -> LinearExpr
scalarTimes r (LinearExpr terms c) =
  mkLinear (map (\(a, b) -> (r*a, b)) terms) (r*c)

getTerm :: String -> LinearExpr -> (Rational, String)
getTerm varName (LinearExpr terms _) =
  fromJust $ find (\(a, b) -> b == varName) terms

minus :: LinearExpr -> LinearExpr -> LinearExpr
minus a b = error "sub"

symRoot :: String -> LinearExpr -> LinearExpr
symRoot varName linExpr =
  let term = getTerm varName linExpr in
   scalarTimes (-1) $ scalarTimes (1 / (fst term)) $ minus linExpr (mkLinear [term] 0)
