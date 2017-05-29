module Utils where

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

updateList :: [a] -> [a] -> Int -> [a]
updateList m x c =
  (take c m) ++ x ++ (drop (c + 1) m)
  
