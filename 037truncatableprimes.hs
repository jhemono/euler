module TruncatablePrimes where
import Data.List (groupBy,intersect)
import IntPrimes (primes)
import Data.Function (on)

order :: Int -> Int
order n = aux n 0
  where aux n' i
          | n' < 10 = i
          | otherwise = aux (n' `div` 10) (i+1)

leftprefix :: Int -> Int
leftprefix n = n `div` 10
rightprefix :: Int -> Int
rightprefix n = n `mod` 10 ^ order n
answer = concat . tail $ zipWith intersect leftsprefixes rightprefixes
looper prefix (g0:g1:gs) = g0 : looper prefix (filter (\p -> prefix p `elem` g0) g1 :gs)
looper _ ([]:_) = []
groups = groupBy ((==) `on` order) primes
leftsprefixes = looper leftprefix groups
rightprefixes = looper rightprefix groups
