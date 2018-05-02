import Data.Char
import System.Environment

splitString :: (Char -> Bool) -> String -> [String]
splitString predicate [] = []
splitString predicate xs = let (word, remaining) = break predicate xs 
                               strippedRemaining = dropWhile predicate remaining
                           in
                               word : splitString predicate strippedRemaining

valueLines :: String -> [[Int]]
valueLines contents = [splitValues line | line <- splitString (=='\n') contents]
                      where splitValues line = [read value :: Int | value <- splitString (=='\t') line]
                               
answer1 :: String -> Int
answer1 contents = sum [maximum values - minimum values | values <- valueLines contents]
             
sumOfEvenDivisions :: [Int] -> Int
sumOfEvenDivisions (value:values) = (sum evenDivisions) + sumOfEvenDivisions values
                                    where evenDivisions = [division | value' <- values, (division, remainder) <- [(max value value') `divMod` (min value value')], remainder == 0]
sumOfEvenDivisions _ = 0
                    
answer2 :: String -> Int
answer2 contents = sum [sumOfEvenDivisions values | values <- valueLines contents]
                   
main :: IO ()
main = do
    part1 <- readFile "day2part1.txt"
    putStrLn . show $ answer1 part1
    part2 <- readFile "day2part2.txt"
    putStrLn . show $ answer2 part2