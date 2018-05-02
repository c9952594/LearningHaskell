import Data.Char
import System.Environment

data Point = Point {x::Int, y::Int}

--grid :: [(Point, Int)]

answer1 :: Int -> Int
answer1 contents = 0
                   
answer2 :: String -> String
answer2 contents = "Answer 2"

main :: IO ()
main = do
    --part1 <- readFile "daypart1.txt"
    putStrLn . show $ answer1 277678
    --part2 <- readFile "daypart2.txt"
    --putStrLn . show $ answer2 part2