import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr $ i + ord 'a'

shift :: Int -> Char -> Char
shift i c | isLower c = int2let $ (i + let2int c) `mod` 26
          | otherwise = c
          
encode :: Int -> String -> String
encode i s = [shift i c | c <- s]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7,
         2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7, 7.5,
         1.9, 0.1, 6.0, 6.3, 9.0,
         2.8, 1.0, 2.4, 0.2, 2.0,
         0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> String
lowers xs = [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = sum [1 | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = length $ lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate i xs = (drop i xs) ++ (take i xs)

positions :: Eq a => a -> [a] -> [Int]
positions f xs = [i | (x, i) <- zip xs [0..], x == f]

crack :: String -> String
crack xs = encode (-factor) xs
           where factor = head (positions (minimum chitab) chitab)
                 chitab = [chisqr (rotate n table') table | n <- [0..25]]
                 table' = freqs xs