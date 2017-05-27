module Linear where

data LinearExpr = LinearExpr [(Rational, String)] Rational
                  deriving (Eq, Ord, Show)

mkLinear terms const = LinearExpr terms const

scalarTimes :: Rational -> LinearExpr -> LinearExpr
scalarTimes r (LinearExpr terms c) = mkLinear (map (\(a, b) -> (r*a, b)) terms) (r*c)
