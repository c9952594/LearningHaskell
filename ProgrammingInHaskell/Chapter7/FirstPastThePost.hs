votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = [a | a <- xs, a < x] ++ [x] ++ [b | b <- xs, b >= x]

results  :: Ord a => [a] -> [(Int,a)]
results vs = sort [(count v vs,v) | v <- removeDuplicates vs]

winner :: Ord a => [a] -> a
winner = snd . last . results
