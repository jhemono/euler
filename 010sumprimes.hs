module SumPrimes where
import Primes(primes)
sumPrimes :: Integer -> Integer
sumPrimes n = sum . takeWhile (< n) $ primes
