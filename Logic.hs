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

