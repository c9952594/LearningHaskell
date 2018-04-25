data Operation = Addition | Subtraction | Multiplication | Division
instance Show Operation where
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"

valid :: Operation -> Int -> Int -> Bool
valid Addition _ _       = True
valid Subtraction x y    = x > y
valid Multiplication _ _ = True
valid Division x y       = x `mod` y == 0

apply :: Operation -> Int -> Int -> Int
apply Addition x y = x + y
apply Subtraction x y = x - y
apply Multiplication x y = x * y
apply Division x y = x `div` y

data Expression = Value Int | Application Operation Expression Expression

instance Show Expression where
  show (Value n) = show n
  show (Application operation left right) = bracket left ++ show operation ++ bracket right
                                            where 
                                              bracket (Value n) = show n
                                              bracket expression = "(" ++ show expression ++ ")"
