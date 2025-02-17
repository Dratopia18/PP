import Data.List (sort)

add5 :: Maybe Int -> Maybe Int
add5 = fmap (+5)

add :: Maybe Int -> Maybe Int -> Maybe Int
add x y = do 
    a <- x
    b <- y
    return (a + b)

sub :: Maybe Int -> Maybe Int -> Maybe Int
sub x y = do
    a <- x
    b <- y
    return (a - b)

mult :: Maybe Int -> Maybe Int -> Maybe Int
mult x y = do
    a <- x
    b <- y
    return (a * b)

main :: IO ()
-- main = putStrLn "Hello, World!"

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- main = do 
--     putStrLn "Choose a number: "
--     n <- getLine
--     let res = fib (read n)
--     putStrLn ("The result is: " ++ show res)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n  = n : collatz (3 * n + 1)

main = do
    putStrLn "Enter a number:"
    n <- fmap read getLine
    let sequence = collatz n
    putStrLn "The Collatz sequence is:"
    print sequence
