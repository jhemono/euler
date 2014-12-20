module Primes (primes, isPrime, primeDivisors, primeDecomposition, divisorsSum, properDivisorsSum,divisors) where
import Data.List (subsequences)

root :: Integer -> Integer
root n = ceiling . (sqrt :: Double -> Double) . fromIntegral $ n

primes :: [Integer]
primes = 2 : 3 : 5 : filter isPrime [7..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `rem` d /= 0) . takeWhile (<= root n) $ primes

primeDivisors :: Integer -> [Integer]
primeDivisors n = filter (\d -> n `rem` d == 0) . takeWhile (<= n `div` 2) $ primes

primeDecomposition :: Integer -> [(Integer, Int)]
primeDecomposition 1 = []
primeDecomposition n = primeCase . map (\p -> (p, factorExponent p n)) $ primeDivisors n
  where factorExponent p n' = case divMod n' p of (n'', 0) -> 1 + factorExponent p n''
                                                  (_, _) -> 0
        primeCase [] = [(n, 1)]
        primeCase xs = xs

-- Faulty
divisors :: Integer -> [Integer]
divisors n = map product . subsequences . concatMap (\(p,i) -> replicate i p) . primeDecomposition $ n

divisorsSum :: Integer -> Integer
divisorsSum n = product . map term . primeDecomposition $ n
  where term (p, i) = ((p ^ (i+1))-1) `div` (p-1)

properDivisorsSum :: Integer -> Integer
properDivisorsSum 0 = 0
properDivisorsSum n = divisorsSum n - n
