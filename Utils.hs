module Utils where

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

updateList :: [a] -> [a] -> Int -> [a]
updateList m x c =
  (take c m) ++ x ++ (drop (c + 1) m)

--[take c (m !! r) ++ x:((m !! r) !! c) ++ drop (c + 1) (m !! r)] ++

insertMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
insertMatrix m x (r,c) =
  take r m ++
  [x:(m !! r)] ++ --([x] ++ (m !! r)) ++ 
  drop (r + 1) m

insertList :: [a] -> [a] -> Int -> [a]
insertList m x c =
  (take c m) ++ x ++ (drop c m)
  
