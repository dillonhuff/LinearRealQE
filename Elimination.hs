module Elimination where

import Control.Exception.Base
import Data.List

import Linear
import Logic
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

signRow :: String -> Int -> LinearExpr -> Int -> [Sign]
signRow varName numIntervals expr i =
  let t = fst $ getTerm varName expr
      sg = if t > 0 then Pos else if t < 0 then Neg else Zero
      nsg = if sg == Pos then Neg else Pos
      before = replicate (i) nsg
      after = replicate (numIntervals - i - 1) sg
      sgRow = before ++ [Zero] ++ after in
   assert ((length sgRow) == numIntervals) sgRow

buildSigns :: String -> Int -> [LinearExpr] -> [[Sign]]
buildSigns varName numIntervals es =
  let rootInds = [1..(length es)] in
   zipWith (signRow varName numIntervals) es rootInds

flipRowsAndCols :: [[a]] -> [[a]]
flipRowsAndCols as =
  if (length $ head as) == 0 then [] else
    let flippedRow = map head as
        rest = map tail as in
     flippedRow:(flipRowsAndCols rest)

tableForRootOrder :: String -> Order LinearExpr -> SignTable
tableForRootOrder varName order =
  let es = extractElems order
      ints = buildIntervals es in
   mkTable es ints (flipRowsAndCols $ buildSigns varName (length ints) es)

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

rootOrderFormula varName order = T

foldAnds :: [Formula a] -> Formula a
foldAnds [] = T
foldAnds [a] = a
foldAnds (a:as) = And a (foldAnds as)
  
toFormula varName f ord st =
  if formulaIsSAT st f then rootOrderFormula varName ord else F

project :: String -> Formula LinearExpr -> Formula LinearExpr
project varName f =
  let exprs = nub $ collectFormulas f
      orders = buildOrders exprs
      sts = map (\ord -> (ord, tableForRootOrder varName ord)) orders in
   foldAnds $ map (\(ord, st) -> toFormula varName f ord st) sts
