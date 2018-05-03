import Data.Char
import System.Environment

answer :: Int -> [Char] -> Int
answer offset xs = sum [d | (d, d') <- zip digits digits', d == d']
                   where digits = [ord x - 48 | x <- xs]
                         digits' = drop offset (cycle digits)

answer1 :: [Char] -> Int
answer1 = answer 1
                   
answer2 :: [Char] -> Int
answer2 digits = answer ((length digits) `div` 2) digits

main :: IO ()
main = do
    contents <- readFile "data.txt"
    putStrLn . show $ answer1 contents
    putStrLn . show $ answer2 contents
