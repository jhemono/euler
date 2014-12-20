module NonAbundantSum(nonSumOfAbundant) where
import Primes(properDivisorsSum)

isAbundant :: Integer -> Bool
isAbundant n = properDivisorsSum n > n

abundantNumbers :: [Integer]
abundantNumbers = filter isAbundant [12..]

abundantSums :: Integer -> [(Integer, Integer)]
abundantSums n = [(a,b) | a <- takeWhile (<= n - 12) abundantNumbers, let b = n - a, a <= b, isAbundant b]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

nonSumOfAbundant :: [Integer]
nonSumOfAbundant = [1..23] ++ filter noSum [25..28123]
  where noSum n = isEmpty $ abundantSums n
