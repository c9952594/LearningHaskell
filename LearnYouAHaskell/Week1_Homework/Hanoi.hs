type Disks = Integer
type Peg = String
type Move = (Peg, Peg)
hanoi :: Disks -> Peg -> Peg -> Peg -> [Move]

hanoi 1 start end _ = [(start, end)]
hanoi n start end temp =
    let nMinusOne = n - 1
    in hanoi nMinusOne start temp end ++
       hanoi 1 start end temp ++
       hanoi nMinusOne temp end start