quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lower ++ [x] ++ quicksort upper
    where lower = [a | a <- xs, a <= x]
          upper = [b | b <- xs, b > x]
          
quicksortReverse :: Ord a => [a] -> [a]
quicksortReverse [] = []
quicksortReverse (x:xs) = quicksortReverse upper ++ [x] ++ quicksortReverse lower
    where lower = [a | a <- xs, a <= x]
          upper = [b | b <- xs, b > x]