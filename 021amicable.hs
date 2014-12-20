module Amicable where
import Primes (properDivisorsSum)
import Data.List (nub)

racine :: Int -> Int
racine = floor . (sqrt :: Double -> Double) . fromIntegral

divisors :: Int -> [Int]
divisors n = (1 :) $ nub $ concat [ [x, n `div` x] | x <- [2..limit], n `rem` x == 0]
  where limit = racine n

isAmicable :: Integer -> Bool
isAmicable a = a /= b && properDivisorsSum b == a
  where b = properDivisorsSum a

sumAmicable :: Integer -> Integer
sumAmicable n = sum . filter isAmicable $ [1..n]
