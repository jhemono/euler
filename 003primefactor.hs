module Main (main) where

root :: Integer -> Integer
root n = ceiling . (sqrt :: Double -> Double) . fromIntegral $ n

primes :: [Integer]
primes = 2 : 3 : 5 : filter isPrime [6..]
  where isPrime n = all (\d -> n `rem` d /= 0) . takeWhile (<= root n) $ primes

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = foldr max 0 . filter divides . takeWhile (<= root n) $ primes
  where divides k = n `mod` k == 0

main :: IO ()
main = interact (show . largestPrimeFactor . read)
