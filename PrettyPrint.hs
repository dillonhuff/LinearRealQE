module PrettyPrint(sumList) where

sumList :: String -> (a -> String) -> [a] -> String
sumList s p [] = ""
sumList s p [x] = p x
sumList s p (x:y:[]) = p x ++ " " ++ s ++ " " ++ p y
sumList s p (x:y:rest) = p x ++ " " ++ s ++ " " ++ p y ++ " " ++ s ++ " " ++ (sumList s p rest)

