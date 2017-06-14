module FerranteRackoff where

import Data.List

import Linear
import Logic

ferranteRackoff :: Formula LinearExpr -> Formula LinearExpr
ferranteRackoff f =
  let fs = nub $ collectFormulas f in
   f
