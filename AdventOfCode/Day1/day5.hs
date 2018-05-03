import Data.Char
import Data.List
import MyLib

data State = 
    State Index Instructions 
    | Jumper Index Instructions
    deriving Show
type Index = Int
type Instructions = [Int]

nextState :: State -> State
nextState (State index (instruction:instructions)) = 
    let index' = index + instruction
        instruction' = instruction + 1
        instructions' = rotate instruction (instruction':instructions)
    in  
        (State index' instructions')
nextState (Jumper index (instruction:instructions)) = 
    let index' = index + instruction
        instruction' = if (instruction >= 3) then instruction - 1 else instruction + 1
        instructions' = rotate instruction (instruction':instructions)
    in  
        (Jumper index' instructions')

exitStates :: State -> [State]
exitStates this = this:(exitStates $ nextState this)
        
answer1 :: Instructions -> [State]
answer1 instructions = 
    let numberOfInstructions = length instructions
    in  takeWhile (\(State index _) -> index >= 0 && index < numberOfInstructions) (exitStates (State 0 instructions))
                   
answer2 :: Instructions -> [State]
answer2 instructions = 
    let numberOfInstructions = length instructions
    in  takeWhile (\(Jumper index _) -> index >= 0 && index < numberOfInstructions) (exitStates (Jumper 0 instructions))

main :: IO ()
main = do
    fileContents <- readFile "day5.txt"
    let values = mySplit (=='\n') (read) fileContents
    --putStrLn . show $ length (answer1 values)
    putStrLn . show $ length (answer2 values)