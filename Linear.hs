module Linear where

import Data.Maybe
import Data.List

import PrettyPrint

data LinearExpr = LinearExpr [(Rational, String)] Rational
                  deriving (Eq, Ord)

instance Show LinearExpr where
  show lx = printLinearExpr lx

showTerm (a, b) = show a ++ "*" ++ b

printLinearExpr (LinearExpr lx 0) = sumList "+" showTerm lx
printLinearExpr (LinearExpr lx c) = sumList "+" showTerm lx ++ " + " ++ show c

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
  let newC = c + r
      newTerms = a ++ q in
   mkLinear newTerms newC

minus a b = plus a (scalarTimes (-1) b)

symRoot :: String -> LinearExpr -> LinearExpr
symRoot varName linExpr =
  let term = getTerm varName linExpr in
   scalarTimes (-1) $ scalarTimes (1 / (fst term)) $ minus linExpr (mkLinear [term] 0)

data Order a = Value a | Less a (Order a) | Equal a (Order a) deriving (Eq, Ord, Show)

allOrders :: [a] -> [Order a]
allOrders [] = []
allOrders [a] = [Value a]
allOrders as =
  let perms = permutations as in
   concatMap buildOrders perms

buildOrders :: [a] -> [Order a]
buildOrders [] = []
buildOrders [a] = [Value a]
buildOrders (a:as) =
  let ords = buildOrders as in
   (map (Less a) ords) ++ (map (Equal a) ords)

extractElems (Value a) = [a]
extractElems (Equal a or) = a:(extractElems or)
extractElems (Less a or) = a:(extractElems or)

lastVal (Value a) = a
lastVal (Less a _) = a
lastVal (Equal a _) = a

isConstant (LinearExpr [] c) = True
isConstant _ = False

constant (LinearExpr _ c) = c


linearizeOrder :: Order a -> [[a]]
linearizeOrder (Value a) = [[a]]
linearizeOrder (Less a ord) = ([a]):(linearizeOrder ord)
linearizeOrder (Equal a ord) =
  let lo = linearizeOrder ord in
   (a:(head lo)):(tail lo)
