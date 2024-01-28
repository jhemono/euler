fibonacciList :: [Integer]
fibonacciList = 1 : 1 : zipWith (+) fibonacciList (tail fibonacciList)

sumEvenFibonacci :: Integer -> Integer
sumEvenFibonacci n = sum . filter even . takeWhile (< n) $ fibonacciList

main :: IO ()
main = interact (show . (sumEvenFibonacci :: Integer -> Integer) . read)
