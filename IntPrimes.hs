module IntPrimes (primes, isPrime, primeDivisors, primeDecomposition, divisorsSum, properDivisorsSum,divisors) where
import Data.List (subsequences)

root :: Int -> Int
root n = ceiling . (sqrt :: Double -> Double) . fromIntegral $ n

primes :: [Int]
primes = 2 : 3 : 5 : filter isPrime [7,9..]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not . any (\d -> n `mod` d == 0) . takeWhile (<= root n) $ primes

primeDivisors :: Int -> [Int]
primeDivisors n = filter (\d -> n `rem` d == 0) . takeWhile (<= n `div` 2) $ primes

primeDecomposition :: Int -> [(Int, Int)]
primeDecomposition 1 = []
primeDecomposition n = primeCase . map (\p -> (p, factorExponent p n)) $ primeDivisors n
  where factorExponent p n' = case divMod n' p of (n'', 0) -> 1 + factorExponent p n''
                                                  (_, _) -> 0
        primeCase [] = [(n, 1)]
        primeCase xs = xs

-- Faulty
divisors :: Int -> [Int]
divisors n = map product . subsequences . concatMap (\(p,i) -> replicate i p) . primeDecomposition $ n

divisorsSum :: Int -> Int
divisorsSum n = product . map term . primeDecomposition $ n
  where term (p, i) = ((p ^ (i+1))-1) `div` (p-1)

properDivisorsSum :: Int -> Int
properDivisorsSum 0 = 0
properDivisorsSum n = divisorsSum n - n
