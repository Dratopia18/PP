fact :: Int -> Int
fact = \n -> if n == 0 then 1 else n * fact (n - 1)

mygcd :: Int -> Int -> Int
mygcd a b = if b == 0 then a else mygcd b (a `mod` b)

myMin :: Int -> Int -> Int
myMin a b = if a < b then a else b

myMax :: Int -> Int -> Int
myMax a b = if a > b then a else b


unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

fun :: [Int] -> [String] 
fun [] = []
fun xs = [case (mod x 3, mod x 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    (_, _) -> show x | x <- xs]
