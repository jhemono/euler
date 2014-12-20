module CircularPrimes (answer) where
import IntPrimes

order :: Int -> Int
order n = aux n 0
  where aux n' i
          | n' <= 10 = i
          | otherwise = aux (n' `div` 10) (i+1)

rotations :: Int -> [Int]
rotations n = take (i+1) $ iterate rotate n
  where i = order n
        rotate n' = let (q,r) = n' `divMod` 10
                    in r * 10^i + q

-- Assumes n is prime
isCircularPrime :: Int -> Bool
isCircularPrime n = all isPrime rs
  where (_:rs) = rotations n

answer :: Int
answer = length . filter isCircularPrime . takeWhile (<=1000000) $ primes
