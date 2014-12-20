module ThousandOnePrime where

root :: Integer -> Integer
root n = ceiling . (sqrt :: Double -> Double) . fromIntegral $ n

primes :: [Integer]
primes = 2 : 3 : 5 : filter isPrime [7..]
  where isPrime n = all (\d -> n `rem` d /= 0) . takeWhile (<= root n) $ primes

thousandOnePrime :: Integer
thousandOnePrime = primes !! (10001 - 1)
