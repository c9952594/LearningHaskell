import Data.Char
import Data.List
import MyLib

answer1 :: String -> String
answer1 contents = "Answer 1"
                   
--answer2 :: String -> String
--answer2 contents = "Answer 2"

main :: IO ()
main = do
    part1 <- readFile "daypart1.txt"
    putStrLn . show $ answer1 part1
    --part2 <- readFile "daypart2.txt"
    --putStrLn . show $ answer2 part2