module Logic where

data Comparison = EQL | LESS | GREATER deriving (Eq, Ord, Show)

data Formula a =
  And (Formula a) (Formula a) |
  Or (Formula a) (Formula a) |
  Not (Formula a) |
  Atom Comparison a |
  T |
  F
  deriving (Eq, Ord, Show)

collectFormulas (And l r) = (collectFormulas l) ++ (collectFormulas r)
collectFormulas (Or l r) = (collectFormulas l) ++ (collectFormulas r)
collectFormulas (Not l) = (collectFormulas l)
collectFormulas (Atom _ l) = [l]
collectFormulas _ = []

showFm (And a b) = "(" ++ (showFm a) ++ " ^ " ++ (showFm b) ++ ")"
showFm (Or a b) = "(" ++ showFm a ++ " v " ++ showFm b ++ ")"
showFm (Not a) = "~(" ++ showFm a ++ ")"
showFm (Atom EQL a) = "(" ++ show a ++ " = 0)"
showFm (Atom LESS a) = "(" ++ show a ++ " < 0)"
showFm (Atom GREATER a) = "(" ++ show a ++ " > 0)"
showFm T = "True"
showFm F = "False"
