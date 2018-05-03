module MyLib (
    mySplit,
    rotate
) where

mySplit :: (Char -> Bool) -> (String -> a) -> String -> [a]
mySplit _ _ [] = [] 
mySplit predicate transformation  xs = let (word, remaining) = break predicate xs 
                                           strippedRemaining = dropWhile predicate remaining
                                       in
                                           (transformation word) : mySplit predicate transformation strippedRemaining

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = if n < 0
              then rotate ((length xs) + n) xs
              else zipWith const (drop n (cycle xs)) xs