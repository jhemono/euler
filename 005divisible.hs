module EvenlyDivisible where

smallestEvenlyDivisible :: Integral a => [a] -> a
smallestEvenlyDivisible = foldl1 lcm

main :: IO ()
main = interact (show . (smallestEvenlyDivisible :: [Integer] -> Integer) . (\n -> [1..n]) . read)
