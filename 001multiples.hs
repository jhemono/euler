module Multiples (main, sumMultiples) where

sumMultiples :: Integer -> Integer
sumMultiples n = sum [i | i <- [1..999], i `mod` 3 == 0 || i `mod` 5 == 0]

main = interact (show . sumMultiples . read)
