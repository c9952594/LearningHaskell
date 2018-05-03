import Data.List



answer1 :: String -> Int
answer1 contents = sum [maximum values - minimum values | values <- allValues]
                   where allValues = mySplit (=='\n') (mySplit (=='\t') (read)) contents
             
sumOfEvenDivisions :: [Int] -> Int
sumOfEvenDivisions (value:values) = (sum evenDivisions) + sumOfEvenDivisions values
                                    where evenDivisions = [division | value' <- values, (division, remainder) <- [(max value value') `divMod` (min value value')], remainder == 0]
sumOfEvenDivisions _ = 0
                    
answer2 :: String -> Int
answer2 contents = sum [sumOfEvenDivisions values | values <- allValues]
                   where allValues = mySplit (=='\n') (mySplit (=='\t') (read)) contents

main :: IO ()
main = do
    part1 <- readFile "data1.txt"
    putStrLn . show $ answer1 part1
    part2 <- readFile "data2.txt"
    putStrLn . show $ answer2 part2