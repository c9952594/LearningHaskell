quicksortDeduplicate :: Ord a => [a] -> [a]
quicksortDeduplicate [] = []
quicksortDeduplicate (x:xs) = quicksortDeduplicate lower ++ [x] ++ quicksortDeduplicate upper
    where lower = [a | a <- xs, a < x]
          upper = [b | b <- xs, b > x]