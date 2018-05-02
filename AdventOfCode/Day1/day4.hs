import Data.Char
import Data.List
import MyLib

hasDuplicates :: (String -> String -> Bool) -> [String] -> Bool
hasDuplicates predicate (word:words) = (any (==True) [predicate word word' | word' <- words]) || hasDuplicates predicate words
hasDuplicates _ _ = False

answer1 :: [[String]] -> Int
answer1 lines = sum [1 | line <- lines, (hasDuplicates (==) line) == False]
       
answer2 :: [[String]] -> Int
answer2 lines = sum [1 | line <- lines, (hasDuplicates (\x y -> (sort x) == (sort y)) line) == False]

main :: IO ()
main = do
    fileContents <- readFile "day4.txt"
    let values = splitValues (=='\n') (==' ') (id) fileContents
    putStrLn . show $ answer1 values
    putStrLn . show $ answer2 values