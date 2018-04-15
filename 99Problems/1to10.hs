myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = last xs

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

elementAt :: Int -> [a] -> a
elementAt n xs = head [x | (x, i) <- (zip xs [1..]), n == i] 

myLength :: [a] -> Int
myLength xs = sum [1 | _ <- xs]

myReverse :: [a] -> [a]
myReverse list = myReverse' list []
               where myReverse' [] reversed = reversed
                     myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
