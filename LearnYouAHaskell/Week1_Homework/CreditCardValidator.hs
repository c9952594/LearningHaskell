toDigits :: Integer -> [Integer]
toDigits cardNumber 
    | 0 >= cardNumber = []
    | otherwise = toDigits remaining ++ [digit]
    where digit = cardNumber `mod` 10
          remaining = cardNumber `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev cardNumber 
    | 0 >= cardNumber = []
    | otherwise = digit : toDigitsRev remaining
    where digit = cardNumber `mod` 10
          remaining = cardNumber `div` 10
          
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : (y * 2) : doubleEveryOther xs
doubleEveryOther (x:xs) = x : doubleEveryOther xs
doubleEveryOther _ = []

sumDigits :: [Integer] -> Integer
sumDigits digits = sum $ map sum $ map toDigits digits

validate :: Integer -> Bool
validate cardNumber = (sumDigits $ doubleEveryOther $ toDigitsRev cardNumber) `mod` 10 == 0 