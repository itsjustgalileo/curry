-- Solution to modules exercise
import Factorial(factorial)

-- Solution to sum of squares
sumOfSquares n = sum [n*n | n <- [0..n]]

-- Solution to Fibonacci sequence
fib n | n == 0 = 0 | n == 1 = 1 | otherwise = fib(n - 1) + fib(n -2)
fibs n = [fib n | n <- [0..n]] 
fibseq = 0 : 1 : zipWith (+) fibseq (tail fibseq)

-- Solution to high-order functions exercise
high :: (Int -> Int) -> Int -> Int
high f n = f . f $ n

-- Solution to recusion-exercise
countDown :: Int -> [Int]
countDown n | n == 0 = [0] | otherwise = n : countDown (n - 1)

-- Solution to list comprehension
evenNums = [0, 2..100]
evens = [n | n <- [0..100], mod n 2 == 0]

-- Solution to Pattern matching
describeList :: [Int] -> [Char]
describeList (x:xs)
    | null [x] = "empty list"
    | otherwise = "list contains: " ++ show (length (x:xs)) ++ " elements"

-- Solution to custom data types exercise
data Point = Point Int Int
    deriving Show

-- Solution to Monads exercise
division x y
         | y == 0 = Nothing
         | otherwise = Just (x/y)

main :: IO()
main = interact $ show . sum . map read . words
