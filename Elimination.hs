module Elimination where

import Control.Exception.Base
import Data.List
import Data.Maybe

import Linear
import Logic
import SignTable

middleIntervals :: [String] -> [Interval]
middleIntervals [x] = [Point $ Var x]
middleIntervals (x:y:xs) =
  [Point $ Var x, Range (Var x) (Var y)] ++ (middleIntervals (y:xs))

--buildIntervals :: [LinearExpr] -> [Interval]
buildIntervals :: LinOrder LinearExpr -> [Interval]
buildIntervals ord =
  let vars = map (\v -> "x" ++ show v) $ [1..(length ord)] --[1..(length $ linearizeOrder ord)]
      negInf = Range NInf (Var $ head vars)
      pInf = Range (Var $ last vars) Inf in
   (negInf:(middleIntervals vars)) ++ [pInf]

signRow :: String -> Int -> LinearExpr -> Int -> [Sign]
signRow varName numIntervals expr i =
  let t = fst $ getTerm varName expr
      sg = if t > 0 then Pos else if t < 0 then Neg else Zero
      nsg = if sg == Pos then Neg else Pos
      before = replicate (i) nsg
      after = replicate (numIntervals - i - 1) sg
      sgRow = before ++ [Zero] ++ after in
   assert ((length sgRow) == numIntervals) sgRow

findRootIndex :: LinearExpr -> [[LinearExpr]] -> Int
findRootIndex l ord =
  fromJust $ findIndex (\group -> elem l group) ord

buildSigns :: String -> LinOrder LinearExpr -> [Interval] -> [[Sign]]
buildSigns varName order intervals =
  let es = extractElems order
      rootInds = map (\p -> findRootIndex p order) es in
   zipWith (signRow varName (length intervals)) es rootInds

                                     -- let rootInds = [1..(length es)] in
  --  zipWith (signRow varName numIntervals) es rootInds

flipRowsAndCols :: [[a]] -> [[a]]
flipRowsAndCols as =
  if (length $ head as) == 0 then [] else
    let flippedRow = map head as
        rest = map tail as in
     flippedRow:(flipRowsAndCols rest)

tableForRootOrder :: String -> LinOrder LinearExpr -> SignTable
tableForRootOrder varName order =
  let es = extractElems order
      ints = buildIntervals order
      signs = flipRowsAndCols $ buildSigns varName order ints in
   mkTable es ints signs

formulaIsSAT :: SignTable -> Formula LinearExpr -> Bool
formulaIsSAT st fm =
  length (satIntervals st fm) > 0

satIntervals st (Atom EQL x) =
  intervalsWithSign x [Zero] st
satIntervals st (Atom GREATER x) =
  intervalsWithSign x [Pos] st
satIntervals st (Atom LESS x) =
  intervalsWithSign x [Neg] st
satIntervals st (And l r) =
  intersect (satIntervals st l) (satIntervals st r)
satIntervals st (Or l r) =
  union (satIntervals st l) (satIntervals st r)
satIntervals st (Not r) =
  intervals st \\ (satIntervals st r)

satIntervalsWithPolys p st (Atom EQL x) =
  if elem x p then intervalsWithSign x [Zero] st else intervals st
satIntervalsWithPolys p st (Atom GREATER x) =
  if elem x p then intervalsWithSign x [Pos] st else intervals st
satIntervalsWithPolys p st (Atom LESS x) =
  if elem x p then intervalsWithSign x [Neg] st else intervals st
satIntervalsWithPolys p st (And l r) =
  intersect (satIntervalsWithPolys p st l) (satIntervalsWithPolys p st r)
satIntervalsWithPolys p st (Or l r) =
  union (satIntervalsWithPolys p st l) (satIntervalsWithPolys p st r)
satIntervalsWithPolys p st (Not r) =
  intervals st \\ (satIntervalsWithPolys p st r)

rootOrdForm :: String -> Order LinearExpr -> Formula LinearExpr
rootOrdForm varName (Value a) = T
rootOrdForm varName (Less a (Value b)) =
  Atom LESS (minus (symRoot varName a) (symRoot varName b))
rootOrdForm varName (Equal a (Value b)) =
  Atom EQL (minus (symRoot varName a) (symRoot varName b))
rootOrdForm varName (Less a or) =
  And (Atom LESS (minus (symRoot varName a) (symRoot varName (lastVal or)))) (rootOrdForm varName or)
rootOrdForm varName (Equal a or) =
  And (Atom EQL (minus (symRoot varName a) (symRoot varName (lastVal or)))) (rootOrdForm varName or)

rootOrderFormula :: String -> LinOrder LinearExpr -> Formula LinearExpr
rootOrderFormula var ord = T
  -- let lOrd = toOrderData ord in
  --  rootOrdForm var lOrd

foldAnds :: [Formula a] -> Formula a
foldAnds [] = T
foldAnds [a] = a
foldAnds (a:as) = And a (foldAnds as)

foldOrs :: [Formula a] -> Formula a
foldOrs [] = T
foldOrs [a] = a
foldOrs (a:as) = Or a (foldOrs as)

toFormula varName f ord st =
  if formulaIsSAT st f then rootOrderFormula varName ord else F

evaluateCmp comp rational =
  if comp == EQL
  then (if rational == 0 then T else F)
  else if comp == LESS then (if rational < 0 then T else F)
       else (if rational > 0 then T else F)

simplifyFm :: Formula LinearExpr -> Formula LinearExpr
simplifyFm (Or F F) = F
simplifyFm (Or F a) = a
simplifyFm (Or a F) = a
simplifyFm (Or T _) = T
simplifyFm (Or _ T) = T
simplifyFm (And T T) = T
simplifyFm (And a T) = a
simplifyFm (And T a) = a
simplifyFm (And F _) = F
simplifyFm (And _ F) = F
simplifyFm (Atom cmp e) =
  if isConstant e then evaluateCmp cmp (constant e) else Atom cmp e
simplifyFm fm = fm

simplifyFmRec (Or a b) = simplifyFm $ Or (simplifyFmRec a) (simplifyFmRec b)
simplifyFmRec (And a b) = simplifyFm $ And (simplifyFmRec a) (simplifyFmRec b)
simplifyFmRec (Not a) = simplifyFm $ Not (simplifyFmRec a)
simplifyFmRec a = simplifyFm a

project :: String -> Formula LinearExpr -> Formula LinearExpr
project varName f =
  let exprs = nub $ collectFormulas f
      orders = allOrders exprs
      sts = map (\ord -> (ord, tableForRootOrder varName ord)) orders in
   --error $ show $ map (\(ord, st) -> toFormula varName f ord st) sts
   simplifyFmRec $ foldOrs $ map (\(ord, st) -> toFormula varName f ord st) sts
